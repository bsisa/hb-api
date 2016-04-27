/**
 *
 */
package ch.bsisa.hyperbird.actview.actors

import ch.bsisa.hyperbird.actview.{ Destination, GetDestination, GetPosition, GetPositionToDestination, Position, SetDestination }
import ch.bsisa.hyperbird.actview.controllers.ActviewApi
import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.util.ElfinUtil

import akka.actor.{ Actor, ActorLogging, Props }
import play.api.libs.json.Json

/**
 * @author Patrick Refondini
 *
 */
class ObjectActor(objectId: String, fleetName: String, startPosition: POINT, elfin: ELFIN) extends Actor with ActorLogging {

  import context._

  var objectPosition: POINT = startPosition
  var objectDestination: Option[POINT] = None
  val serverNotification = ActviewApi.getServerNotification()

  val driverActor = actorOf(Props[DriverActor], name = objectId + "-driver")

  def receive = {
    case message: String =>
      objectPosition match {
        case POINT(curr_oos, Some(curr_x), Some(curr_y), curr_z, curr_ksi, curr_angle, curr_alpha, curr_xs, curr_ys, curr_zs, curr_ksis, curr_angles, curr_alphas, curr_id, curr_id_g, _, curr_objClass, curr_group, curr_remark) =>
          objectDestination match {
            case Some(POINT(pos, Some(x), Some(y), z, ksi, angle, alpha, xs, ys, zs, ksis, angles, alphas, id, id_g, _, objClass, group, remark)) =>
              log.info(s"ObjectActor objectId=$objectId has (x,y,z) position ($curr_x, $curr_y, $curr_z) and destination ($x, $y, $z). Message: $message")
            case None =>
              log.info(s"ObjectActor objectId=$objectId has (x,y,z) position ($curr_x, $curr_y, $curr_z) and NO destination. Message: $message")
          }
      }
    case GetDestination =>
      val message = objectDestination match {
        case Some(POINT(pos, Some(x), Some(y), z, ksi, angle, alpha, xs, ys, zs, ksis, angles, alphas, id, id_g, _, objClass, group, remark)) =>
          s"ObjectActor objectId=$objectId has (x,y,z) destination ($x, $y, $z)."
        case None =>
          s"ObjectActor objectId=$objectId has no destination."
      }
      log.info(s"ObjectActor sending : $message to serverNotification")
      serverNotification ! message

    case SetDestination(newDestination) =>
      objectDestination = newDestination
      driveToDestination(objectPosition, objectDestination)

    // Position computed from DriverActor
    case Position(objectId, newPosition) =>
      objectPosition = newPosition
      driveToDestination(objectPosition, objectDestination)
      notifyCurrentPosition()

    case GetPosition =>
      notifyCurrentPosition()

  }

  def driveToDestination(currentPosition: POINT, destinationOpt: Option[POINT]) = {

    // Only proceed a destination exist
    destinationOpt.foreach { destination =>
      // Consider destination reached given x, y coordinates
      if (objectPosition.X.get == destination.X.get && objectPosition.Y.get == destination.Y.get) {
        objectDestination = None
        log.info(s">>>>    Object $objectId reached destination <<<<");
      } else {
        driverActor ! GetPositionToDestination(objectPosition, destination)
      }

    }
  }

  /**
   * Provide sever side event notification to JS clients.
   */
  def notifyCurrentPosition() = {
    val elfinWithUpdatedPosition = ElfinUtil.updateElfinForme( elfin, FORME(Seq(objectPosition), Seq(), Seq(), Seq()))
    val elfinJs = ch.bsisa.hyperbird.model.format.ElfinFormat.toJson(elfinWithUpdatedPosition)
    val messageToSend = Json.obj("group" -> fleetName, "text" -> "position", "user" -> "server", "time" -> new java.util.Date(), "elfin" -> elfinJs)
    serverNotification ! messageToSend
  }

}
