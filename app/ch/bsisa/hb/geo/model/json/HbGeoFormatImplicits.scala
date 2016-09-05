package ch.bsisa.hb.geo.model.json

import play.api.libs.json.{ Reads, Writes, Format, JsPath }
import play.api.libs.functional.syntax._
import ch.bsisa.hb.geo.model.{ Bounds, BoundsRequest, Coordinates, CoordinatesWithId, Pixels, PixelsCoordinates }

/**
 * Contains formatters to deal with conversion between model
 * objects and their corresponding JSON structure.
 *
 * @author Patrick Refondini
 */
object HbGeoFormatImplicits {

  val coordinatesReads: Reads[Coordinates] = (
    (JsPath \ "xEastingLng").read[Double] and
    (JsPath \ "yNorthingLat").read[Double] and
    (JsPath \ "zAltitude").read[Double])(Coordinates.apply _)

  val coordinatesWrites: Writes[Coordinates] = (
    (JsPath \ "xEastingLng").write[Double] and
    (JsPath \ "yNorthingLat").write[Double] and
    (JsPath \ "zAltitude").write[Double])(unlift(Coordinates.unapply))

  /**
   * Provides automatic conversion from `Coordinates` to `JSON`
   * and from `JSON` to `Coordinates`.
   *
   * Example JSON:
   * {{{
   * {
   *     "xEastingLng" : "556580.0",
   *     "yNorthingLat" : "177850.0",
   *     "zAltitude" : "477.0"
   * }
   * }}}
   *
   */
  implicit val coordinatesFormat: Format[Coordinates] = Format(coordinatesReads, coordinatesWrites)

  val coordinatesWithIdReads: Reads[CoordinatesWithId] = (
    (JsPath \ "id").read[String] and
    (JsPath \ "coord").read[Coordinates])(CoordinatesWithId.apply _)

  val coordinatesWithIdWrites: Writes[CoordinatesWithId] = (
    (JsPath \ "id").write[String] and
    (JsPath \ "coord").write[Coordinates])(unlift(CoordinatesWithId.unapply))

  /**
   * Provides automatic conversion from `CoordinatesWithId` to `JSON`
   * and from `JSON` to `CoordinatesWithId`.
   *
   * Example JSON:
   * {{{
   * {
   *     "id" : "G20050725170559296",
   *     "coord" : {
   *         "xEastingLng" : "556580.0",
   *         "yNorthingLat" : "177850.0",
   *         "zAltitude" : "477.0"
   *     }
   * }
   * }}}
   *
   */
  implicit val coordinatesWithIdFormat: Format[CoordinatesWithId] = Format(coordinatesWithIdReads, coordinatesWithIdWrites)

  val boundsReads: Reads[Bounds] = (
    (JsPath \ "swCoord").read[Coordinates] and
    (JsPath \ "neCoord").read[Coordinates])(Bounds.apply _)

  val boundsWrites: Writes[Bounds] = (
    (JsPath \ "swCoord").write[Coordinates] and
    (JsPath \ "neCoord").write[Coordinates])(unlift(Bounds.unapply))

  /**
   * Provides automatic conversion from `Bounds` to `JSON`
   * and from `JSON` to `Bounds`.
   *
   * Example JSON:
   * {{{
   * {
   *     "swCoord" : {
   *         "xEastingLng" : "556580.0",
   *         "yNorthingLat" : "177850.0",
   *         "zAltitude" : "477.0"
   *     },
   *     "neCoord" : {
   *         "xEastingLng" : "557580.0",
   *         "yNorthingLat" : "176850.0",
   *         "zAltitude" : "477.0"
   *     }
   * }
   * }}}
   *
   */
  implicit val boundsFormat: Format[Bounds] = Format(boundsReads, boundsWrites)

  val pixelsReads: Reads[Pixels] = (
    (JsPath \ "x").read[Int] and
    (JsPath \ "y").read[Int])(Pixels.apply _)

  val pixelsWrites: Writes[Pixels] = (
    (JsPath \ "x").write[Int] and
    (JsPath \ "y").write[Int])(unlift(Pixels.unapply))

  /**
   * Provides automatic conversion from `Pixels` to `JSON`
   * and from `JSON` to `Pixels`.
   *
   * Example JSON:
   * {{{
   * {
   *     "x" : 100,
   *     "y" : 467
   * }
   * }}}
   *
   */
  implicit val pixelsFormat: Format[Pixels] = Format(pixelsReads, pixelsWrites)

  val pixelsCoordinatesReads: Reads[PixelsCoordinates] = (
    (JsPath \ "pixels").read[Pixels] and
    (JsPath \ "coord").read[Coordinates])(PixelsCoordinates.apply _)

  val pixelsCoordinatesWrites: Writes[PixelsCoordinates] = (
    (JsPath \ "pixels").write[Pixels] and
    (JsPath \ "coord").write[Coordinates])(unlift(PixelsCoordinates.unapply _))

  /**
   * Provides automatic conversion from `PixelsCoordinates` to `JSON`
   * and from `JSON` to `PixelsCoordinates`.
   *
   * Example JSON:
   * {{{
   * {
   *     "pixels" : {
   *         "x" : "640",
   *         "y" : "723"
   *     },
   *     "coord" : {
   *         "xEastingLng" : "557580.0",
   *         "yNorthingLat" : "176850.0",
   *         "zAltitude" : "477.0"
   *     }
   * }
   * }}}
   *
   */
  implicit val pixelsCoordinatesFormat: Format[PixelsCoordinates] = Format(pixelsCoordinatesReads, pixelsCoordinatesWrites)


  val boundsRequestReads: Reads[BoundsRequest] = (
    (JsPath \ "rasterImgSize").read[Pixels] and
    (JsPath \ "record1").read[PixelsCoordinates] and
    (JsPath \ "record2").read[PixelsCoordinates])(BoundsRequest.apply _)

  val boundsRequestWrites: Writes[BoundsRequest] = (
    (JsPath \ "rasterImgSize").write[Pixels] and
    (JsPath \ "record1").write[PixelsCoordinates] and
    (JsPath \ "record2").write[PixelsCoordinates])(unlift(BoundsRequest.unapply _))

  /**
   * Provides automatic conversion from `BoundsRequest` to `JSON`
   * and from `JSON` to `BoundsRequest`.
   *
   * Example JSON:
   * {{{
   *{
   *  "rasterImgSize" : {
   *    "x" : "640",
   *     "y" : "723"
   *  },
   *  "record1" {
   *    "pixels" : {
   *      "x" : "640",
   *      "y" : "723"
   *     },
   *     "coord" : {
   *       "xEastingLng" : "557580.0",
   *       "yNorthingLat" : "176850.0"
   *     }
   * 	},
   *  "record2" {
   *    "pixels" : {
   *      "x" : "640",
   *      "y" : "723"
   *    },
   *    "coord" : {
   *      "xEastingLng" : "557580.0",
   *      "yNorthingLat" : "176850.0"
   *    }
   *  }
   *}
   * }}}
   *
   */
  implicit val boundsRequestFormat: Format[BoundsRequest] = Format(boundsRequestReads, boundsRequestWrites)

}
