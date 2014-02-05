package ch.bsisa.hyperbird.model.format

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._

import play.api.libs.json.{ Json, JsValue }

/**
 * Functions helping to read and write ELFIN model object
 * to and from JSON and XML serialisation formats.
 *
 * @author Patrick Refondini
 *
 */
object ElfinFormat {

  def toXml(elfin: ELFIN): scala.xml.Node = {
    val nodeSeq = scalaxb.toXML[ELFIN](elfin, None, Some("ELFIN"), ch.bsisa.hyperbird.model.proto.defaultScope)
    nodeSeq(0)
  }

  def fromXml(elfinXml: scala.xml.Elem): ELFIN = scalaxb.fromXML[ELFIN](elfinXml)

  def toJson(elfin: ELFIN): JsValue = Json.toJson(elfin)

  def fromJson(elfinJson: JsValue): ELFIN = elfinJson.as[ELFIN]

}