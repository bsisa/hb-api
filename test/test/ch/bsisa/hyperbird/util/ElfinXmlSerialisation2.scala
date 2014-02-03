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

//  val etat1MixedContent = elfin.CARACTERISTIQUE.get.ETAT.get.ETAT1.get.mixed
//  val expectedEtat1MixedContent = """Etat1 TEst 1, chemin des|Tests 7,
//				chemin
//				des|Tests 9, chemin des...
//			"""

//  "elfinTest001Xml to Scala object ELFIN " should {
//    s"have elfin.CARACTERISTIQUE.get.ETAT.get.ETAT1.get.mixed equal to expectedEtat1MixedContent" in {
//      getMixedContent(etat1MixedContent) must equalTo(expectedEtat1MixedContent)
//    }
//  }

  // Print JSON to file

  // ================================================================== 
  // ch.bsisa.hyperbird.model.ELFIN => play.api.libs.json.JsValue 
  // ==================================================================

  // Produce JSON from ELFIN object
  //val elfinJson = Json.toJson(elfin)

  val mutationsJson = Json.toJson(elfin.MUTATIONS)
  val geoselectionJson = Json.toJson(elfin.GEOSELECTION)
  val identifiantJson = Json.toJson(elfin.IDENTIFIANT)
  val caracteristiqueJson = Json.toJson(elfin.CARACTERISTIQUE)
  val partenaireJson = Json.toJson(elfin.PARTENAIRE)
  val activiteJson = Json.toJson(elfin.ACTIVITE)
  //val formeJson = Json.toJson(elfin.FORME)
  val annexeJson = Json.toJson(elfin.ANNEXE)
  val diversJson = Json.toJson(elfin.DIVERS)

  // ==================================================================
  // play.api.libs.json.JsValue => JSON file
  // ==================================================================

  //JsonXmlConverter.printJsonToFile(elfinJson, TestResultsDir + "ELFINResult2.json")

  JsonXmlConverter.printJsonToFile(mutationsJson, TestResultsDir + "MUTATIONSResult2.json")
  JsonXmlConverter.printJsonToFile(geoselectionJson, TestResultsDir + "GEOSELECTIONResult2.json")
  JsonXmlConverter.printJsonToFile(identifiantJson, TestResultsDir + "IDENTIFIANTResult2.json")
  JsonXmlConverter.printJsonToFile(caracteristiqueJson, TestResultsDir + "CARACTERISTIQUEResult2.json")
  JsonXmlConverter.printJsonToFile(partenaireJson, TestResultsDir + "PARTENAIREResult2.json")
  JsonXmlConverter.printJsonToFile(activiteJson, TestResultsDir + "ACTIVITEResult2.json")
  //JsonXmlConverter.printJsonToFile(formeJson, TestResultsDir + "FORMEResult2.json")
  JsonXmlConverter.printJsonToFile(annexeJson, TestResultsDir + "ANNEXEResult2.json")
  JsonXmlConverter.printJsonToFile(diversJson, TestResultsDir + "DIVERSResult2.json")

  // ==================================================================
  // JSON file => play.api.libs.json.JsValue
  // ==================================================================
  //val elfinJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "ELFINResult2.json")

  val mutationsJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "MUTATIONSResult2.json")
  val geoselectionJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "GEOSELECTIONResult2.json")
  val identifiantJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "IDENTIFIANTResult2.json")
  val caracteristiqueJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "CARACTERISTIQUEResult2.json")
  val partenaireJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "PARTENAIREResult2.json")
  val activiteJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "ACTIVITEResult2.json")
  //val formeJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "FORMEResult2.json")
  val annexeJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "ANNEXEResult2.json")
  val diversJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "DIVERSResult2.json")

  // ==================================================================
  // play.api.libs.json.JsValue => ch.bsisa.hyperbird.model.ELFIN
  // ==================================================================
  //val elfinFromFile = elfinJsonInput.as[ELFIN]

  val mutationsFromFile = mutationsJsonInput.as[MUTATIONS]
  val geoselectionFromFile = geoselectionJsonInput.as[GEOSELECTION]
  val identifiantFromFile = identifiantJsonInput.as[IDENTIFIANT]
  val caracteristiqueFromFile = caracteristiqueJsonInput.as[CARACTERISTIQUE]
  val partenaireFromFile = partenaireJsonInput.as[PARTENAIRE]
  val activiteFromFile = activiteJsonInput.as[ACTIVITE]
  //val formeFromFile = formeJsonInput.as[FORME]
  val annexeFromFile = annexeJsonInput.as[ANNEXE]
  val diversFromFile = diversJsonInput.as[DIVERS]

  //TODO: add tests checking the expected values are found in :
  // elfinFromFile and its sub elements
  // mutationsFromFile 
  // geoselectionFromFile
  // identifiantFromFile
  // caracteristiqueFromFile
  // partenaireFromFile
  // activiteFromFile
  // formeFromFile
  // annexeFromFile
  // diversFromFile  
  
  // ==================================================================
  // ch.bsisa.hyperbird.model.ELFIN => scala.xml.Elem
  // ==================================================================  
  // Convert Scala objects back to XML without data loss nor structure 
  // change.
  //val elfinBackToXml = scalaxb.toXML[ELFIN](elfinFromFile, None, Some("ELFIN"), ch.bsisa.hyperbird.model.proto.defaultScope)
  val elfinBackToXml = scalaxb.toXML[ELFIN](elfin, None, Some("ELFIN"), ch.bsisa.hyperbird.model.proto.defaultScope)

  "The elfinBackToXml (scala.xml.Elem) " should {
    s"have Id equal to ${expectedElfinTest002_Id}" in {
      (elfinBackToXml \ "@Id").text must equalTo(expectedElfinTest002_Id)
    }
    s"have ID_G equal to ${expectedElfinTest002_ID_G}" in {
      (elfinBackToXml \ "@ID_G").text must equalTo(expectedElfinTest002_ID_G)
    }
  }

  // ==================================================================
  // scala.xml.Elem => XML file
  // ==================================================================
  // Dump the XML to file for manual review
  scala.xml.XML.save(TestResultsDir + "scalaxbResult2.xml", elfinBackToXml.iterator.next)

}