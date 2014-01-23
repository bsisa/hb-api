package test.ch.bsisa.hyperbird.util

import org.specs2.mutable._
import play.api.libs.json.Json
import ch.bsisa.hyperbird.util.JsonXmlConverter
import scala.xml.XML
import play.api.libs.json.JsSuccess
import play.api.libs.json.JsError

class JsonXmlConverterSpec extends Specification {

  // G20040930101030005/G20040203114894000
  val TestDir = "./test/resources/"

  val elfinTest001Xml = XML.loadFile(TestDir + "elfin-test-001.xml")
  //  val source = scala.io.Source.fromFile(TestDir + "elfin-test-001.json")
  //  val elfinTest001JsonString = source.getLines mkString "\n"
  //  source.close()

  println("=================================================================================== ")
  println("================================= CONVERSION ====================================== ")
  println("=================================================================================== ")
  //println("elfinXmlElem: " + elfinXmlElem)
  //println(JsonXmlConverter.xmlElemToJson(elfinTest001Xml))
  //println("elfinJsonString: " + elfinJsonString)
  println("=================================================================================== ")
  println("================================= CONVERSION ====================================== ")
  println("=================================================================================== ")

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
  
  //println("elfinTest001JsValueBackToXmlString: " + elfinTest001JsValueBackToXmlString)
  
  scala.xml.XML.save("./test/resources/result.xml", elfinTest001JsValueBackToXml.iterator.next)

  // Make sure information found in original XML we load 
  // is also found in JSON back to XML outcome
  "The elfinTest001JsValueBackToXml (scala.xml.Elem) " should {
    s"have Id equal to ${expectedElfinTest001_Id}" in {
      (elfinTest001JsValueBackToXml \ "@Id").text must equalTo(expectedElfinTest001_Id)
    }
    s"have ID_G equal to ${expectedElfinTest001_ID_G}" in {
      (elfinTest001JsValueBackToXml \ "@ID_G").text must equalTo(expectedElfinTest001_ID_G)
    }
  }  
  
  
  
  

}