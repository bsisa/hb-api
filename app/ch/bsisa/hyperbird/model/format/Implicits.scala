package ch.bsisa.hyperbird.model.format

import ch.bsisa.hyperbird.model.Elfin
import play.api.libs.json._
import play.api.libs.functional.syntax._

/**
 * Exposes Reads, Writes, Format utilities to allow Elfin
 * object seamless serialisation and deserialisation to JSON format.
 * 
 * @author Patrick Refondini
 */
object Implicits {
  // Note __ is syntax visual simplification for JsPath
  implicit val elfinReads: Reads[Elfin] = (
    (__ \ "Id").read[String] and
    (__ \ "ID_G").read[String] and
    (__ \ "CLASSE").read[String])(Elfin)

}