package ch.bsisa.hyperbird.util.format

import net.liftweb.json._
import net.liftweb.json.JsonAST.render
import play.api.libs.json.Json
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import play.api.Logger

/**
 * Helps converting to and from String, XML and JSON formats.
 *
 * Relies on Scalaxb and Play Json formatting (reads,writes)
 *
 * @see ch.bsisa.hyperbird.model
 * @see ch.bsisa.hyperbird.model.format
 * @see ch.bsisa.hyperbird.model.proto
 *
 * The latter is to become deprecated and shall be removed
 * following attributes to element and empty elements loss of
 * information detected in our XML => JSON => XML use case.
 *
 * <i>Thin layer built on top of net.liftweb.json library.
 * Check https://github.com/lift/framework/tree/master/core/json
 * SBT dependency: "net.liftweb" %% "lift-json" % "2.5"
 * for details.
 *
 * <b>Beware of serious limitations such as loosing empty element
 * and transforming attributes to elements while doing a full
 * XML => JSON => XML lifecyle. Which is by design not a mistake
 * but a loss of information between the two formats {XML,JSON}
 * when no additional meta information is provided.</b>
 * </i>
 *
 * @author Patrick Refondini
 */
object JsonXmlConverter {

  /**
   * @deprecated - Issues with underlying xmlStringToNodeSeq function using XhtmlParser.
   */
  def xmlStringToJson(xmlString: String): String = xmlNodeSeqToJson(xmlStringToNodeSeq(xmlString))

  /**
   * @deprecated - Issues with underlying xmlStringToNodeSeq function using XhtmlParser.
   *
   * Converts an xml input in string format to a sequence of Scala xml nodes.
   *
   * Note: scala.xml.XML.load(xmlString) can return a scala.xml.Element.
   * In some context it may lead to "java.net.MalformedURLException: no protocol"
   * problems. See: http://www.scala-lang.org/old/node/4501.html
   *
   * @param xmlString
   * @return scala.xml.NodeSeq
   *
   */
  def xmlStringToNodeSeq(xmlString: String): scala.xml.NodeSeq = scala.xml.XML.loadString(xmlString)
  //scala.xml.parsing.XhtmlParser(scala.io.Source.fromString(xmlString))

  /**
   * Converts a scala.xml.NodeSeq to a JSON string in a pretty or compact format depending on `pretty` parameter value.
   *
   * @param nodeSeq - sequence of XML nodes
   * @param pretty if true the JSON string will be printed in pretty format
   * @return JSON string
   */
  def xmlNodeSeqToJson(nodeSeq: scala.xml.NodeSeq, pretty: Boolean): String =
    if (pretty) Printer.pretty(render(Xml.toJson(nodeSeq)))
    else compact(render(Xml.toJson(nodeSeq)))

  def xmlNodeSeqToJson(nodeSeq: scala.xml.NodeSeq): String = xmlNodeSeqToJson(nodeSeq, true)

  /**
   * Converts a single scala.xml.Elem to a JSON string in a pretty or compact format depending on `pretty` parameter value.
   *
   * @param elem a XML element
   * @param pretty if true the JSON string will be printed in pretty format
   * @return JSON string
   */
  def xmlElemToJson(elem: scala.xml.Elem, pretty: Boolean): String = xmlNodeSeqToJson(elem, pretty)

  /**
   * Converts a single scala.xml.Elem to a JSON in compact format.
   *
   * @param elem a XML element
   * @return JSON string
   */
  def xmlElemToJson(elem: scala.xml.Elem): String = xmlElemToJson(elem, false)

  /**
   * Converts a sequence of scala.xml.Elem to JSON by simply appending them with a comma separator.
   *
   */
  def xmlSeqToJson(elemSeq: Seq[scala.xml.Elem]): String = {
    val conv = for {
      elem <- elemSeq
    } yield {
      xmlElemToJson(elem)
    }
    conv.mkString(",")
  }

  /**
   * @deprecated - Do not use, has issues loosing data structure and data.
   * Note: Currently use in a single test: elfinTest001JsValueBackToXmlString in JsonXmlConverterSpec
   * Converts a JSON string to XML.
   */
  def jsonStringToXml(jsonStr: String) = {
    Xml.toXml(parse(jsonStr))
  }

  /**
   * Dumps a JsValue to file.
   */
  def printJsonToFile(json: JsValue, filePath: String): Unit = {
    val fileWriter = new java.io.FileWriter(filePath)
    try { fileWriter.write(Json.prettyPrint(json)) } finally { fileWriter.close() }
  }

  /**
   * Loads a JsValue from file.
   */
  def loadJsonFromFile(pathToJsonFile: String): JsValue = {
    val jsonString = scala.io.Source.fromFile(pathToJsonFile).mkString
    Json.parse(jsonString)
  }

}

/**
 * JsonXmlConvert exception class
 */
case class JsonXmlConvertException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
