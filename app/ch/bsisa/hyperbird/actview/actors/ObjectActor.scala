/**
 *
 */
package ch.bsisa.hyperbird.actview.actors

import ch.bsisa.hyperbird.model._

import akka.actor.{ Actor, ActorLogging }

/**
 * @author Patrick Refondini
 *
 */
class ObjectActor(objectId:String, startPosition:POINT) extends Actor with ActorLogging {

  var position : POINT = startPosition  
  var destination : Option[POINT] = None
  
  def receive = {
    case message:String => 
      log.info(s"ObjectActor objectId=$objectId received message: $message")
      // TODO: SetDestination(POINT(pos, Some(x), Some(y), z, ksi, angle, alpha, xs, ys, zs, ksis, angles, alphas, id, id_g, _ , objClass, group, remark))
    case POINT(pos, Some(x), Some(y), z, ksi, angle, alpha, xs, ys, zs, ksis, angles, alphas, id, id_g, _ , objClass, group, remark) => 
      log.info(s"ObjectActor objectId=$objectId received new position (x,y,z) = ($x, $y, $z)")
    // TODO: GetDestination
    //case
    // TODO: GetPosition
    //case 
    
    case _ => log.warning("Received message other than expected String...")
  }
  
}
