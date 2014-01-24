package test.ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.proto._

import org.specs2.mutable._

import scala.xml.XML

/**
* Tests serialisation deserialisation of Elfin object to and from XML format.
 * 
 * @author Patrick Refondini
 */
class ElfinXmlSerialisation extends Specification {
  
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

  // Convert XML to Scala objects
  val elfin = scalaxb.fromXML[ELFIN](elfinTest001Xml)
  
  "The elfin object (ch.bsisa.hyperbird.model.ELFIN) " should {
    s"have Id equal to ${expectedElfinTest001_Id}" in {
      elfin.Id must equalTo(expectedElfinTest001_Id)
    }
    s"have ID_G equal to ${expectedElfinTest001_ID_G}" in {
      elfin.ID_G must equalTo(expectedElfinTest001_ID_G)
    }
  }  
  
  // Convert Scala objects back to XML without data loss nor structure change.
  val elfinBackToXml = scalaxb.toXML[ELFIN](elfin, None, Some("ELFIN"), ch.bsisa.hyperbird.model.proto.defaultScope)
  
    "The elfinBackToXml (scala.xml.Elem) " should {
    s"have Id equal to ${expectedElfinTest001_Id}" in {
      (elfinBackToXml \ "@Id").text must equalTo(expectedElfinTest001_Id)
    }
    s"have ID_G equal to ${expectedElfinTest001_ID_G}" in {
      (elfinBackToXml \ "@ID_G").text must equalTo(expectedElfinTest001_ID_G)
    }
  }
  
  
  // Dump the XML to file for manual review
  scala.xml.XML.save(TestResultsDir + "scalaxbResult.xml", elfinBackToXml.iterator.next)
  
}