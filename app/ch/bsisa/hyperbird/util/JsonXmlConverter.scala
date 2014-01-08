package ch.bsisa.hyperbird.util

import net.liftweb.json._
import net.liftweb.json.JsonAST.render

/**
 * Helps converting to and from XML and JSON formats.
 *
 * Thin layer built on top of net.liftweb.json library.
 * Check https://github.com/lift/framework/tree/master/core/json
 * SBT dependency: "net.liftweb" %% "lift-json" % "2.5"
 * for details.
 */
object JsonXmlConverter {

  /**
   * Converts a single scala.xml.Elem to a JSON string in a pretty or compact format depending on `pretty` parameter value.
   *
   * @param elem an XML element
   * @param pretty if true the JSON string will be printed in pretty format
   * @return JSON string
   */
  def xmlElemToJson(elem: scala.xml.Elem, pretty: Boolean): String =
    if (pretty) Printer.pretty(render(Xml.toJson(elem)))
    else compact(render(Xml.toJson(elem)))

  /**
   * Converts a single scala.xml.Elem to a JSON in compact format.
   *
   * @param elem an XML element
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