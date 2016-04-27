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
  
  def receive = {
    case GetPositionToDestination(position, destination) =>
      val newPosition = computeNextPosition(position, destination)
      // Avoid driver speeding!
      Thread.sleep(2000)
      sender ! Position("na", newPosition)
  }

  def computeNextPosition(currPosition: POINT, destination: POINT): POINT = {
    
    // TODO: Basic test. Compute
    val newX : Double = currPosition.X.get - step
    val newY : Double = currPosition.Y.get + step
    
    val newPosition = POINT(currPosition.POS,
      X = Some(newX), // Update
      Y = Some(newY), // Update
      Z = currPosition.Z,
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
      GROUPE = currPosition.GROUPE,
      REMARQUE = currPosition.REMARQUE)

    newPosition
  }

}
