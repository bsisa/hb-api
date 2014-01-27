package ch.bsisa.hyperbird.model.format

import ch.bsisa.hyperbird.model._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import ch.bsisa.hyperbird.model.SCHEMATIQUE
import ch.bsisa.hyperbird.model.GEOGRAPHIE

/**
 * Exposes Reads, Writes, Format utilities to allow MELFIN, ELFIN
 * object seamless serialisation and deserialisation to JSON format.
 *
 * * First use ScalaJsonInceptions for shortest formatter definition
 * * Second use ScalaJsonCombinators if simple customisation is needed
 * * Fallback to Play plain Json API to deal with varargs constructors parameter
 *
 * @see http://www.playframework.com/documentation/2.2.x/ScalaJsonInceptions
 * @see http://www.playframework.com/documentation/2.2.x/ScalaJsonCombinators
 * @see http://www.playframework.com/documentation/2.2.x/api/scala/index.html#play.api.libs.json.Reads$
 * @see http://www.playframework.com/documentation/2.2.x/api/scala/index.html#play.api.libs.json.Writes$
 * @see http://www.playframework.com/documentation/2.2.x/api/scala/index.html#play.api.libs.json.package
 *
 * @author Patrick Refondini
 */
object Implicits {

  // See: http://www.playframework.com/documentation/2.2.x/ScalaJsonInception
  // TODO: suppres Elfin entity together with corresponding test once ELFIN entity implemented.
  implicit val elfinReads: Reads[Elfin] = Json.reads[Elfin]

  //implicit val MUTATIONReads: Reads[MUTATION] = Json.reads[MUTATION]
  implicit val MUTATIONFormat: Format[MUTATION] = Json.format[MUTATION]

  implicit object MUTATIONSFormat extends Format[MUTATIONS] {
    def reads(json: JsValue): JsResult[MUTATIONS] =
      JsSuccess(MUTATIONS((json \ "MUTATION").as[List[MUTATION]]: _*))
    def writes(ms: MUTATIONS): JsValue = JsObject(
      for (m <- ms.MUTATION) yield "MUTATION" -> Json.toJson(m))
  }

  implicit object TYPETypeFormat extends Format[TYPEType] {

    def reads(json: JsValue): JsResult[TYPEType] =
      (json \ "TYPE") match {
        case JsString(value) => value match {
          case "SCHEMATIQUE" => JsSuccess(SCHEMATIQUE)
          case "GEOGRAPHIE" => JsSuccess(GEOGRAPHIE)
          case invalid => JsError(s"Invalid string value ${invalid} found for TYPE. Valid values are {SCHEMATIQUE,GEOGRAPHIE}")
        }
        case _ => JsError(s"Invalid JsValue type received for TYPE. Expecting JsString only.")
      }

    def writes(t: TYPEType): JsValue = t.toString match {
      case "SCHEMATIQUE" => JsString("SCHEMATIQUE")
      case "GEOGRAPHIE" => JsString("GEOGRAPHIE")
    }
  }

  implicit val CENTROIDEFormat: Format[CENTROIDE] = Json.format[CENTROIDE]

  case class GEOSELECTION(CENTROIDE: ch.bsisa.hyperbird.model.CENTROIDE*)

  implicit object GEOSELECTIONFormat extends Format[GEOSELECTION] {
    def reads(json: JsValue): JsResult[GEOSELECTION] =
      JsSuccess(GEOSELECTION((json \ "CENTROIDE").as[List[CENTROIDE]]: _*))
    def writes(gs: GEOSELECTION): JsValue = JsObject(
      for (c <- gs.CENTROIDE) yield "CENTROIDE" -> Json.toJson(c))
  }

  //  implicit val SCHEMATIQUEFormat: Format[SCHEMATIQUE] = Json.format[SCHEMATIQUE]
  //  implicit val GEOGRAPHIEFormat: Format[GEOGRAPHIE] = Json.format[GEOGRAPHIE]  

  // implicit val GEOSELECTIONFormat: Format[GEOSELECTION] = 
  //  
  //case class GEOSELECTION(CENTROIDE: ch.bsisa.hyperbird.model.CENTROIDE*)
  //
  //case class CENTROIDE(TYPE: ch.bsisa.hyperbird.model.TYPEType,
  //  XM: Double,
  //  YM: Double,
  //  ZM: Option[Double] = None,
  //  RM: Double)
  //  
  //object TYPEType {
  //  def fromString(value: String, scope: scala.xml.NamespaceBinding): TYPEType = value match {
  //    case "GEOGRAPHIE" => GEOGRAPHIE
  //    case "SCHEMATIQUE" => SCHEMATIQUE
  //
  //  }
  //}
  //
  //case object GEOGRAPHIE extends TYPEType { override def toString = "GEOGRAPHIE" }
  //case object SCHEMATIQUE extends TYPEType { override def toString = "SCHEMATIQUE" }

  //implicit val ELFINReads: Reads[ELFIN] = Json.reads[ELFIN]

  //  implicit object MutationFormat extends Format[MUTATION] {
  //
  //    def reads(json: JsValue): JsResult[MUTATION] = MUTATION(
  //      (json \ "DATE").as[String],
  //      (json \ "ROLE").as[String],
  //      (json \ "MOT_DE_PASSE").asOpt[String],
  //      (json \ "UTILISATEUR").asOpt[String])
  //
  //    def writes(m: MUTATION): JsValue = JsObject(List(
  //      "DATE" -> JsString(m.DATE),
  //      "ROLE" -> JsString(m.ROLE),
  //      m.MOT_DE_PASSE match {
  //        case Some(mdp) => ("MOT_DE_PASSE" -> JsString(mdp))
  //        case None => ("MOT_DE_PASSE" -> JsNull)
  //      },
  //      m.UTILISATEUR match {
  //        case Some(user) => ("UTILISATEUR" -> JsString(user))
  //        case None => ("UTILISATEUR" -> JsNull)
  //      }))
  //
  //  }

  //implicit object MutationsFormat: Reads[MUTATIONS] = (
  //    (__ \ "MUTATION").lazyRead( email [MUTATION](mutationReads)) )(MUTATIONS)

  //case class MELFIN(ELFIN: ch.bsisa.hyperbird.model.ELFIN*)
  //    implicit val melfinReads: Reads[MELFIN] = (
  //        (__ \ "ELFIN").read[Seq[ELFIN])(MELFIN)

  //    implicit val elfinReads: Reads[ELFIN] = (
  //    (__ \ "MUTATIONS").read[String] and        
  //    (__ \ "GEOSELECTION").read[String] and
  //    (__ \ "IDENTIFIANT").read[String] and
  //    (__ \ "CARACTERISTIQUE").read[String] and
  //    (__ \ "PARTENAIRE").read[String] and    
  //    (__ \ "ACTIVITE").read[String] and
  //    (__ \ "FORME").read[String] and
  //    (__ \ "ANNEXE").read[String] and
  //    (__ \ "DIVERS").read[String] and    
  //    (__ \ "Id").read[String] and
  //    (__ \ "ID_G").read[String] and
  //    (__ \ "CLASSE").read[String] and 
  //    (__ \ "GROUPE").read[String] and    
  //    (__ \ "TYPE").read[String] and
  //    (__ \ "NATURE").read[String] and
  //    (__ \ "SOURCE").read[String])(ELFIN)        

  //case class ELFIN(MUTATIONS: Option[ch.bsisa.hyperbird.model.MUTATIONS] = None,
  //  GEOSELECTION: Option[ch.bsisa.hyperbird.model.GEOSELECTION] = None,
  //  IDENTIFIANT: Option[ch.bsisa.hyperbird.model.IDENTIFIANT] = None,
  //  CARACTERISTIQUE: Option[ch.bsisa.hyperbird.model.CARACTERISTIQUE] = None,
  //  PARTENAIRE: Option[ch.bsisa.hyperbird.model.PARTENAIRE] = None,
  //  ACTIVITE: Option[ch.bsisa.hyperbird.model.ACTIVITE] = None,
  //  FORME: Option[ch.bsisa.hyperbird.model.FORME] = None,
  //  ANNEXE: Option[ch.bsisa.hyperbird.model.ANNEXE] = None,
  //  DIVERS: Option[ch.bsisa.hyperbird.model.DIVERS] = None,
  //  Id: String,
  //  ID_G: String,
  //  CLASSE: String,
  //  GROUPE: Option[String] = None,
  //  TYPE: Option[ch.bsisa.hyperbird.model.TYPE] = None,
  //  NATURE: String,
  //  SOURCE: Option[String] = None)

}