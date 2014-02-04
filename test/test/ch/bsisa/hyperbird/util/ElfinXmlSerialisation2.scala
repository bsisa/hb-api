package test.ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import org.specs2.mutable._
import scala.xml.XML
import play.api.libs.json.Json
import play.api.libs.json._

/**
 * Tests full ELFIN serialisation deserialisation lifcycle.
 *
 * '''XML to JSON'''
 * - XML file => scala.xml.Elem
 * - scala.xml.Elem => ch.bsisa.hyperbird.model.ELFIN
 * - ch.bsisa.hyperbird.model.ELFIN => play.api.libs.json.JsValue
 * - play.api.libs.json.JsValue => JSON file
 *
 * '''JSON to XML'''
 * - JSON file => play.api.libs.json.JsValue
 * - play.api.libs.json.JsValue => ch.bsisa.hyperbird.model.ELFIN
 * - ch.bsisa.hyperbird.model.ELFIN => scala.xml.Elem
 * - scala.xml.Elem => XML file
 *
 * @author Patrick Refondini
 */
class ElfinXmlSerialisation2 extends BaseSerialisationSpec {

  val expectedElfinTest002_Id = "G20060110115605316"
  val expectedElfinTest002_ID_G = "G20040930101030005"

    
  // ================================================================== 
  // XML file => scala.xml.Elem
  // ==================================================================    
  val elfinTest002Xml = XML.loadFile(TestResourcesDir + "elfin-test-002.xml")

  // Make sure the XML we load contains the information 
  // we want to look for after transformation to JSON  
  "The elfinTest001Xml (scala.xml.Elem) " should {
    s"have Id equal to ${expectedElfinTest002_Id}" in {
      (elfinTest002Xml \ "@Id").text must equalTo(expectedElfinTest002_Id)
    }
    s"have ID_G equal to ${expectedElfinTest002_ID_G}" in {
      (elfinTest002Xml \ "@ID_G").text must equalTo(expectedElfinTest002_ID_G)
    }
  }

  // ================================================================== 
  // scala.xml.Elem => ch.bsisa.hyperbird.model.ELFIN
  // ==================================================================
  val elfin = scalaxb.fromXML[ELFIN](elfinTest002Xml)

  "The elfin object (ch.bsisa.hyperbird.model.ELFIN) " should {
    s"have Id equal to ${expectedElfinTest002_Id}" in {
      elfin.Id must equalTo(expectedElfinTest002_Id)
    }
    s"have ID_G equal to ${expectedElfinTest002_ID_G}" in {
      elfin.ID_G must equalTo(expectedElfinTest002_ID_G)
    }
  }

  // ================================================================== 
  // ch.bsisa.hyperbird.model.ELFIN => play.api.libs.json.JsValue 
  // ==================================================================

  "Json.toJson(elfin.FORME) " should {
    "throw a java.lang.NumberFormatException due to 'NaN' value contained in ANGLES attribute" in {
    	Json.toJson(elfin.FORME) must throwA [java.lang.NumberFormatException] 
    }
  }
  

}
