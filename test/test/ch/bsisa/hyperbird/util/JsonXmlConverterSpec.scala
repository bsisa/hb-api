package test.ch.bsisa.hyperbird.util

import org.specs2.mutable._
import play.api.libs.json.Json
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import scala.xml.XML
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsError

/**
 * @deprecated See ElfinXmlSerialisation instead
 * Tests serialisation deserialisation of Elfin object to and from XML format.
 * 
 * @author Patrick Refondini
 */
class JsonXmlConverterSpec extends Specification {


  val TestResourcesDir = "./test/resources/"
  val TestResultsDir = "./test/results/"
    
  val elfinTest001Xml = XML.loadFile(TestResourcesDir + "elfin-test-001.xml")
  val expectedElfinTest001_Id = "G20040203114894000"    
  val expectedElfinTest001_ID_G = "G20040930101030005"

  // Make sure the XML we load contains the information 
  // we want to look for after transformation to JSON  

  "The elfinTest001Xml (scala.xml.Elem) " should {
    s"have Id equal to ${expectedElfinTest001_Id}" in {
      (elfinTest001Xml \ "@Id").text must equalTo(expectedElfinTest001_Id)
    }
    s"have ID_G equal to ${expectedElfinTest001_ID_G}" in {
      (elfinTest001Xml \ "@ID_G").text must equalTo(expectedElfinTest001_ID_G)
    }
  }

  // Convert XML to JSON
  val elfinTest001JsonString = JsonXmlConverter.xmlElemToJson(elfinTest001Xml)
  val elfinTest001JsValue = Json.parse(elfinTest001JsonString)

  // Check information found in XML is also part of JSON 
  // Note: matching JsResult to JsSuccess/JsError to end up with 
  // the same check condition is done to provide more detailed 
  // information on the failure thanks to extracted exceptions. 
  // To modify once a better solution is found. 
  "The elfinTest001JsValue (play.api.libs.json.JsValue) " should {
    s"have Id equal to ${expectedElfinTest001_Id}" in {
      (elfinTest001JsValue \ "ELFIN" \ "Id").validate[String] match {
        case JsSuccess(value,path) => value must equalTo(expectedElfinTest001_Id) 
        case JsError(exceptions) => exceptions must equalTo(expectedElfinTest001_Id)
      } 
    }
    s"have ID_G equal to ${expectedElfinTest001_ID_G}" in {
      (elfinTest001JsValue \ "ELFIN" \ "ID_G").validate[String] match {
        case JsSuccess(value, path) => value must equalTo(expectedElfinTest001_ID_G)
        case JsError(exceptions) => exceptions must equalTo(expectedElfinTest001_ID_G)
      }
    }
  }

  // Transform JSON back to XML Format
  
  val elfinTest001JsValueBackToXmlString = JsonXmlConverter.jsonStringToXml(elfinTest001JsValue.toString).toString
  val elfinTest001JsValueBackToXml = JsonXmlConverter.xmlStringToNodeSeq(elfinTest001JsValueBackToXmlString)
  
  // Dump the XML to file for manual review.
  // This is an example of XML data structure modification and loss:
  // Attributes Id, ID_G,... are all transformed to XML elements.
  // FORME/POINT elements have disappeared and their attributes flattened. 
  scala.xml.XML.save(TestResultsDir + "wrongResult.xml", elfinTest001JsValueBackToXml.iterator.next)

  // Make sure information found in original XML we load 
  // is also found in JSON back to XML outcome
  // Note: Knowing the structure and information loss of the above
  // procedure let's make our test expect NOT to find the Id and 
  // ID_G information back.
  "The elfinTest001JsValueBackToXml (scala.xml.Elem) " should {
    s"NOT have Id equal to ${expectedElfinTest001_Id} given the known lost XML attribute issue" in {
      (elfinTest001JsValueBackToXml \ "@Id").text must not equalTo(expectedElfinTest001_Id)
    }
    s"but have Id found in new Id element equal to ${expectedElfinTest001_Id} instead" in {
      (elfinTest001JsValueBackToXml \ "Id").text must equalTo(expectedElfinTest001_Id)
    }    
    s"NOT have ID_G equal to ${expectedElfinTest001_ID_G} given the known lost XML attribute issue" in {
      (elfinTest001JsValueBackToXml \ "@ID_G").text must not equalTo(expectedElfinTest001_ID_G)
    }
    s"but have ID_G found in new ID_G element equal to ${expectedElfinTest001_ID_G} instead" in {
      (elfinTest001JsValueBackToXml \ "ID_G").text must equalTo(expectedElfinTest001_ID_G)
    }    
    
  }  
  
  
  
  

}