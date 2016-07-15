/**
 *
 */
package ch.bsisa.hyperbird.actview.actors

import ch.bsisa.hyperbird.actview.{ GetPositionToDestination, Position }
//import ch.bsisa.hyperbird.actview.controllers.ActviewApi
import ch.bsisa.hyperbird.model._

import akka.actor.{ Actor, ActorLogging }
//import play.api.libs.json.Json

/**
 * @author Patrick Refondini
 *
 */
class DriverActor extends Actor with ActorLogging {

  // Moves 
  val step = 10
  val rdm = new java.util.Random
  def receive = {
    case GetPositionToDestination(position, destination) =>
      val newPosition = computeNextPosition(position, destination)
      // Avoid driver speeding!
      val sleepMillis = rdm.nextInt(5) * 250
      log.debug("GetPositionToDestination sleeps for {}", sleepMillis)
      Thread.sleep(sleepMillis)
      sender ! Position("na", newPosition)
  }

  /**
   * Compute next position given current, destination position and step.
   */
  def computeNextPosition(currPosition: POINT, destination: POINT): POINT = {

    // TODO: Basic test. Compute
    val currX: Double = currPosition.X.get
    val currY: Double = currPosition.Y.get
    val destX: Double = destination.X.get
    val destY: Double = destination.Y.get

    val newX = computeNewCoord(curr = currX, dest = destX)
    val newY = computeNewCoord(curr = currY, dest = destY)

    val newPosition = POINT(
      POS = currPosition.POS,
      X = Some(newX), // Update
      Y = Some(newY), // Update
      Z = currPosition.Z,
      XG = currPosition.XG, 
      YG = currPosition.YG, 
      ZG = currPosition.ZG,      
      KSI = currPosition.KSI,
      ANGLE = currPosition.ANGLE,
      ALPHA = currPosition.ALPHA,
      XS = currPosition.XS,
      YS = currPosition.YS,
      ZS = currPosition.ZS,
      KSIS = currPosition.KSIS,
      ANGLES = currPosition.ANGLES,
      ALPHAS = currPosition.ALPHAS,
      Id = currPosition.Id,
      ID_G = currPosition.ID_G,
      FONCTION = currPosition.FONCTION,
      CLASSE = currPosition.CLASSE,
      GROUPE = currPosition.GROUPE)//,
      //REMARQUE = currPosition.REMARQUE)

    newPosition
  }
  
  /**
   * Basic move algo.
   */
  def computeNewCoord(curr:Double,dest:Double) = {
    if ((dest - curr).abs < 2 * step) {
      dest
    } else if (dest < curr) {
      curr - step
    } else { curr + step }
  }

}
