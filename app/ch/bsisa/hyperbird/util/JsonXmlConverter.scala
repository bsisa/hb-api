package ch.bsisa.hyperbird.util

import net.liftweb.json._
import net.liftweb.json.JsonAST.render

/**
 * Helps converting to and from String, XML and JSON formats.
 *
 * Thin layer built on top of net.liftweb.json library.
 * Check https://github.com/lift/framework/tree/master/core/json
 * SBT dependency: "net.liftweb" %% "lift-json" % "2.5"
 * for details.
 *
 * @author Patrick Refondini
 */
object JsonXmlConverter {

  
  def xmlStringToJson(xmlString: String) : String = xmlNodeSeqToJson(xmlStringToNodeSeq(xmlString))
  
  /**
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
  def xmlStringToNodeSeq(xmlString: String): scala.xml.NodeSeq =
    scala.xml.parsing.XhtmlParser(scala.io.Source.fromString(xmlString))

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
   * Converts a JSON string to XML.
   */
  def jsonStrintToXml(jsonStr: String) = {
    Xml.toXml(parse(jsonStr))
  }

}