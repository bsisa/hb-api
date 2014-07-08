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
class ElfinXmlSerialisation extends BaseSerialisationSpec {


  val expectedElfinTest001_Id = "G20040931234567890"
  val expectedElfinTest001_ID_G = "G20040930101030005"
  val expectedELFIN_IDENTIFIANT_VALEUR = 0.0
  val expectedELFIN_IDENTIFIANT_VALEUR_A_NEUF =  8600.0

  // ================================================================== 
  // XML file => scala.xml.Elem
  // ==================================================================    
  val elfinTest001Xml = XML.loadFile(TestResourcesDir + "elfin-test-001.xml")

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

  // ================================================================== 
  // scala.xml.Elem => ch.bsisa.hyperbird.model.ELFIN
  // ==================================================================
  val elfin = scalaxb.fromXML[ELFIN](elfinTest001Xml)

  "The elfin object (ch.bsisa.hyperbird.model.ELFIN) " should {
    s"have Id equal to ${expectedElfinTest001_Id}" in {
      elfin.Id must equalTo(expectedElfinTest001_Id)
    }
    s"have ID_G equal to ${expectedElfinTest001_ID_G}" in {
      elfin.ID_G must equalTo(expectedElfinTest001_ID_G)
    }
    s"have ELFIN.IDENTIFIANT.VALEUR equal to ${expectedELFIN_IDENTIFIANT_VALEUR}" in {
      elfinFromFile.IDENTIFIANT.get.VALEUR.get must be equalTo (expectedELFIN_IDENTIFIANT_VALEUR)
    }
    s"have ELFIN.IDENTIFIANT.VALEUR_A_NEUF equal to ${expectedELFIN_IDENTIFIANT_VALEUR_A_NEUF}" in {
      elfinFromFile.IDENTIFIANT.get.VALEUR_A_NEUF.get must be equalTo (expectedELFIN_IDENTIFIANT_VALEUR_A_NEUF)
    }    
  }

  val etat1MixedContent = elfin.CARACTERISTIQUE.get.ETAT.get.ETAT1.get.mixed
  val expectedEtat1MixedContent = """Etat1 TEst 1, chemin des|Tests 7,
				chemin
				des|Tests 9, chemin des...
			"""

  "elfinTest001Xml to Scala object ELFIN " should {
    s"have elfin.CARACTERISTIQUE.get.ETAT.get.ETAT1.get.mixed equal to expectedEtat1MixedContent" in {
      getMixedContent(etat1MixedContent) must equalTo(expectedEtat1MixedContent)
    }
  }

  // Print JSON to file

  // ================================================================== 
  // ch.bsisa.hyperbird.model.ELFIN => play.api.libs.json.JsValue 
  // ==================================================================

  // Produce JSON from ELFIN object
  val elfinJson = Json.toJson(elfin)

  val mutationsJson = Json.toJson(elfin.MUTATIONS)
  val geoselectionJson = Json.toJson(elfin.GEOSELECTION)
  val identifiantJson = Json.toJson(elfin.IDENTIFIANT)
  val caracteristiqueJson = Json.toJson(elfin.CARACTERISTIQUE)
  val partenaireJson = Json.toJson(elfin.PARTENAIRE)
  val activiteJson = Json.toJson(elfin.ACTIVITE)
  val formeJson = Json.toJson(elfin.FORME)
  val annexeJson = Json.toJson(elfin.ANNEXE)
  val diversJson = Json.toJson(elfin.DIVERS)

  // ==================================================================
  // play.api.libs.json.JsValue => JSON file
  // ==================================================================

  JsonXmlConverter.printJsonToFile(elfinJson, TestResultsDir + "ELFINResult.json")

  JsonXmlConverter.printJsonToFile(mutationsJson, TestResultsDir + "MUTATIONSResult.json")
  JsonXmlConverter.printJsonToFile(geoselectionJson, TestResultsDir + "GEOSELECTIONResult.json")
  JsonXmlConverter.printJsonToFile(identifiantJson, TestResultsDir + "IDENTIFIANTResult.json")
  JsonXmlConverter.printJsonToFile(caracteristiqueJson, TestResultsDir + "CARACTERISTIQUEResult.json")
  JsonXmlConverter.printJsonToFile(partenaireJson, TestResultsDir + "PARTENAIREResult.json")
  JsonXmlConverter.printJsonToFile(activiteJson, TestResultsDir + "ACTIVITEResult.json")
  JsonXmlConverter.printJsonToFile(formeJson, TestResultsDir + "FORMEResult.json")
  JsonXmlConverter.printJsonToFile(annexeJson, TestResultsDir + "ANNEXEResult.json")
  JsonXmlConverter.printJsonToFile(diversJson, TestResultsDir + "DIVERSResult.json")

  // ==================================================================
  // JSON file => play.api.libs.json.JsValue
  // ==================================================================
  val elfinJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "ELFINResult.json")

  val mutationsJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "MUTATIONSResult.json")
  val geoselectionJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "GEOSELECTIONResult.json")
  val identifiantJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "IDENTIFIANTResult.json")
  val caracteristiqueJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "CARACTERISTIQUEResult.json")
  val partenaireJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "PARTENAIREResult.json")
  val activiteJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "ACTIVITEResult.json")
  val formeJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "FORMEResult.json")
  val annexeJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "ANNEXEResult.json")
  val diversJsonInput = JsonXmlConverter.loadJsonFromFile(TestResultsDir + "DIVERSResult.json")

  // ==================================================================
  // play.api.libs.json.JsValue => ch.bsisa.hyperbird.model.ELFIN
  // ==================================================================
  val elfinFromFile = elfinJsonInput.as[ELFIN]

  val mutationsFromFile = mutationsJsonInput.as[MUTATIONS]
  val geoselectionFromFile = geoselectionJsonInput.as[GEOSELECTION]
  val identifiantFromFile = identifiantJsonInput.as[IDENTIFIANT]
  val caracteristiqueFromFile = caracteristiqueJsonInput.as[CARACTERISTIQUE]
  val partenaireFromFile = partenaireJsonInput.as[PARTENAIRE]
  val activiteFromFile = activiteJsonInput.as[ACTIVITE]
  val formeFromFile = formeJsonInput.as[FORME]
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
  val elfinBackToXml = scalaxb.toXML[ELFIN](elfinFromFile, None, Some("ELFIN"), ch.bsisa.hyperbird.model.proto.defaultScope)
  //val elfinBackToXml = scalaxb.toXML[ELFIN](elfin, None, Some("ELFIN"), ch.bsisa.hyperbird.model.proto.defaultScope)

  "The elfinBackToXml (scala.xml.Elem) " should {
    s"have Id equal to ${expectedElfinTest001_Id}" in {
      (elfinBackToXml \ "@Id").text must equalTo(expectedElfinTest001_Id)
    }
    s"have ID_G equal to ${expectedElfinTest001_ID_G}" in {
      (elfinBackToXml \ "@ID_G").text must equalTo(expectedElfinTest001_ID_G)
    }
  }

  // ==================================================================
  // scala.xml.Elem => XML file
  // ==================================================================
  // Dump the XML to file for manual review
  scala.xml.XML.save(TestResultsDir + "scalaxbResult.xml", elfinBackToXml.iterator.next)

}