package ch.bsisa.hyperbird.model.format

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import play.api.libs.json.{ Json, JsObject, JsValue }
import play.api.Logger
import ch.bsisa.hyperbird.model.MELFIN
import play.api.libs.json.JsArray
import ch.bsisa.hyperbird.model.CARACTERISTIQUE

/**
 * Functions helping to read and write ELFIN, MELFIN model object
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

  def toXml(melfin: MELFIN): scala.xml.Node = {
    val nodeSeq = scalaxb.toXML[MELFIN](melfin, None, Some("MELFIN"), ch.bsisa.hyperbird.model.proto.defaultScope)
    nodeSeq(0)
  }

  def fromXml(elfinXmlElem: scala.xml.Elem): ELFIN = scalaxb.fromXML[ELFIN](elfinXmlElem)

  def fromXml(elfinXmlNode: scala.xml.Node): ELFIN = {
    try {
    	scalaxb.fromXML[ELFIN](elfinXmlNode)
    } catch { // Encapsulate all exception within our own adding specific info regarding failing ELFIN conversion.
      case exception: Throwable =>
        val elfinId = (elfinXmlNode \ "@Id").text
        val elfinID_G = (elfinXmlNode \ "@ID_G").text
        Logger.error(s"${exception} with elfin: ${elfinID_G} / ${elfinId} ")
        throw ElfinFormatException(s"ELFIN with ID_G: ${elfinID_G} and Id: ${elfinId} failed to deserialise from XML to ELFIN Scala object.", exception, elfinId, elfinID_G)
    }

  }

  def caracteristiquefromXml(elfinXmlElem: scala.xml.Elem): CARACTERISTIQUE = scalaxb.fromXML[CARACTERISTIQUE](elfinXmlElem)
  
  def toJson(elfin: ELFIN): JsValue = {
    try {
      Json.toJson(elfin)
    } catch { // Encapsulate all exception within our own adding specific info regarding failing ELFIN conversion.
      case exception: Throwable =>
        Logger.error(s"${exception} with elfin: ${elfin.ID_G} / ${elfin.Id} ")
        throw ElfinFormatException(s"ELFIN with ID_G: ${elfin.ID_G} and Id: ${elfin.Id} failed to serialise ELFIN Scala object to JSON", exception, elfin.Id, elfin.ID_G)
    }
  }

  def fromJson(elfinJson: JsValue): ELFIN = elfinJson.as[ELFIN]

  /**
   * Creates a sequence of ELFIN objects from an XML element (MELFIN)
   * containing a list of ELFIN XML elements.
   */
  def elfinsFromXml(melfinElem: scala.xml.Elem): Seq[ELFIN] = {
    // Unwrap wrap tag (should be MELFIN)
    val elfinNodeSeq = melfinElem \\ "ELFIN"

    // Convert XML to scala objects
    val elfins = for { elfinNode <- elfinNodeSeq } yield fromXml(elfinNode)
    elfins
  }

  /**
   * Formats a sequence of ELFIN objects to a sequence of Json values
   * each one being the Json serialisation of its corresponding ELFIN.
   */
  def elfinsToJson(elfins: Seq[ELFIN]): Seq[JsValue] = {
    //val elfinsJsonArray = Json.arr(elfins)
    val elfinsJson = for { elfin <- elfins } yield toJson(elfin)
    elfinsJson
  }

  /**
   * Formats a sequence of ELFIN objects to a JsValue which is a JsArray 
   * of JSON elfins.
   */
  def elfinsToJsonArray(elfins: Seq[ELFIN]): JsValue = {
    val elfinsSeqJsValue = elfinsToJson(elfins)
    val elfinsJsArrayValue =  new JsArray (elfinsSeqJsValue) 
    //val elfinsJsArrayValue = Json.toJson(elfins)
    elfinsJsArrayValue
  }

  def elfinsJsonToMelfinJson(elfinsJson: Seq[JsValue]): JsObject = {
    // Package Seq[JsValue] where each JsValue is an elfin as a MELFIN
    Json.obj("MELFIN" -> elfinsJson)
  }

  /**
   * Transforms a scala.xml.Elem MELFIN expected to contain
   * a sequence of ELFIN elements to a Json object containing
   * an array of ELFIN Json objects.
   */
  def elfinsXmlToJson(melfinElem: scala.xml.Elem): JsObject = {

    // Convert XML to scala objects
    val elfins = elfinsFromXml(melfinElem)

    // Convert Scala objects to JSON
    val elfinsJson = elfinsToJson(elfins)

    elfinsJsonToMelfinJson(elfinsJson)
  }

//  case class ElfinException(message: String = null, cause: Throwable = null, elfinId: String = "", elfinID_G : String = "") extends Exception(message, cause) 
  
  /**
   * ElfinFormat exception class
   */
  case class ElfinFormatException(message: String = null, cause: Throwable = null, elfinId: String = "", elfinID_G : String = "") extends Exception(message, cause)
}