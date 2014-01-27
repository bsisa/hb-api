package ch.bsisa.hyperbird.model.format

import ch.bsisa.hyperbird.model._
import play.api.libs.json._
import play.api.libs.json.Reads._ // Makes list, seq, ... combinators keywords available 
import play.api.libs.functional.syntax._

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

  //  case class User(id: Long, name: String, friends: List[User])
  //
  //  implicit object UserFormat extends Format[User] {
  //    def reads(json: JsValue): JsResult[User] = JsSuccess(User(
  //      (json \ "id").as[Long],
  //      (json \ "name").as[String],
  //      (json \ "friends").asOpt[List[User]].getOrElse(List())))
  //
  //    def writes(u: User): JsValue = JsObject(List(
  //      "id" -> JsNumber(u.id),
  //      "name" -> JsString(u.name),
  //      "friends" -> JsArray(u.friends.map(fr => JsObject(List("id" -> JsNumber(fr.id),
  //        "name" -> JsString(fr.name)))))))
  //  }

  // See: JSON API http://www.playframework.com/documentation/2.2.x/api/scala/index.html#play.api.libs.json.package  
  //  case class User(id: Long, name: String, friends: List[User])

  //  implicit val userFormat = Json.format[User]

  //  implicit object UserFormat extends Format[User] {
  //    def reads(json: JsValue): JsResult[User] = JsSuccess(User(
  //      (json \ "id").as[Long],
  //      (json \ "name").as[String],
  //      (json \ "friends").asOpt[List[User]].getOrElse(List())))
  //
  //    def writes(u: User): JsValue = JsObject(List(
  //      "id" -> JsNumber(u.id),
  //      "name" -> JsString(u.name),
  //      "friends" -> JsArray(u.friends.map(fr => JsObject(List("id" -> JsNumber(fr.id),
  //        "name" -> JsString(fr.name)))))))
  //  }  

  // See: http://www.playframework.com/documentation/2.2.x/ScalaJsonCombinators
  // Note __ is syntax visual simplification for JsPath
  //  implicit val elfinReads: Reads[Elfin] = (
  //    (__ \ "Id").read[String] and
  //    (__ \ "ID_G").read[String] and
  //    (__ \ "CLASSE").read[String])(Elfin)

  // See: http://www.playframework.com/documentation/2.2.x/ScalaJsonInception
  implicit val elfinReads: Reads[Elfin] = Json.reads[Elfin]

  

  implicit val MUTATIONReads: Reads[MUTATION] = Json.reads[MUTATION]

  implicit object MUTATIONSReads extends Reads[MUTATIONS] {
    def reads(json: JsValue): JsResult[MUTATIONS] =
      JsSuccess(MUTATIONS((json \ "MUTATION").as[List[MUTATION]]: _*))
  }

  //implicit val ELFINReads: Reads[ELFIN] = Json.reads[ELFIN]

  //case class MUTATIONS(MUTATION: ch.bsisa.hyperbird.model.MUTATION*)

  //case class MUTATION(DATE: String,
  //  ROLE: String,
  //  MOT_DE_PASSE: Option[String] = None,
  //  UTILISATEUR: Option[String] = None)  

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