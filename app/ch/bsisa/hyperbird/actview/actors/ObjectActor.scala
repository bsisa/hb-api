/**
 *
 */
package ch.bsisa.hyperbird.actview.actors

import ch.bsisa.hyperbird.actview.{ Destination, GetDestination, GetPosition, Position, SetDestination }
import ch.bsisa.hyperbird.actview.controllers.ActviewApi
import ch.bsisa.hyperbird.model._

import akka.actor.{ Actor, ActorLogging }
import play.api.libs.json.Json

/**
 * @author Patrick Refondini
 *
 */
class ObjectActor(objectId: String, startPosition: POINT, elfin: ELFIN) extends Actor with ActorLogging {

  //import context.parent

  var position: POINT = startPosition
  var destination: Option[POINT] = None
  val serverNotification = ActviewApi.getServerNotification()

  def receive = {
    case message: String =>
      position match {
        case POINT(curr_oos, Some(curr_x), Some(curr_y), curr_z, curr_ksi, curr_angle, curr_alpha, curr_xs, curr_ys, curr_zs, curr_ksis, curr_angles, curr_alphas, curr_id, curr_id_g, _, curr_objClass, curr_group, curr_remark) =>
          destination match {
            case Some(POINT(pos, Some(x), Some(y), z, ksi, angle, alpha, xs, ys, zs, ksis, angles, alphas, id, id_g, _, objClass, group, remark)) =>
              log.info(s"ObjectActor objectId=$objectId has (x,y,z) position ($curr_x, $curr_y, $curr_z) and destination ($x, $y, $z). Message: $message")
            case None =>
              log.info(s"ObjectActor objectId=$objectId has (x,y,z) position ($curr_x, $curr_y, $curr_z) and NO destination. Message: $message")
          }
      }
    case GetDestination =>
      val message = destination match {
        case Some(POINT(pos, Some(x), Some(y), z, ksi, angle, alpha, xs, ys, zs, ksis, angles, alphas, id, id_g, _, objClass, group, remark)) =>
          s"ObjectActor objectId=$objectId has (x,y,z) destination ($x, $y, $z)."
        case None =>
          s"ObjectActor objectId=$objectId has no destination."
      }
      log.info(s"ObjectActor sending : $message to serverNotification")
      serverNotification ! message
    case SetDestination(position) => destination = Some(position)
    case GetPosition =>
      val message = position match {
        case POINT(pos, Some(x), Some(y), z, ksi, angle, alpha, xs, ys, zs, ksis, angles, alphas, id, id_g, _, objClass, group, remark) =>
          s"ObjectActor objectId=$objectId has (x,y,z) position ($x, $y, $z)."
      }
      val elfinJs = ch.bsisa.hyperbird.model.format.ElfinFormat.toJson(elfin)
      val messageToSend = Json.obj("group" -> "Fleet1", "text" -> "position", "user" ->  "server", "time" -> new java.util.Date(), "elfin" -> elfinJs )
      log.info(s"ObjectActor sending : $messageToSend to serverNotification")
      serverNotification ! messageToSend
    //sender ! Position(objectId, position)
  }

}
