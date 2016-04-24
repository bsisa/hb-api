/**
 *
 */
package ch.bsisa.hyperbird.actview.actors

import akka.actor.{ Actor, ActorRef, ActorLogging, Props }
import akka.routing.BroadcastRouter
import akka.routing.RoundRobinRouter
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._

/**
 * @author Patrick Refondini
 *
 */
class FleetActor(name: String, colour: String) extends Actor with ActorLogging {

  import context._

  var fleetOpt: Option[Vector[ActorRef]] = None

  
  // TODO: load obj infor from database 
  def createFleet(): Vector[ActorRef] = {
    
    val obj1ActName = s"${name}-1"
    val obj2ActName = s"${name}-2"
    val obj3ActName = s"${name}-3"
    val obj1Act = actorOf(Props(new ObjectActor(objectId = obj1ActName)), name = obj1ActName)
    val obj2Act = actorOf(Props(new ObjectActor(objectId = obj2ActName)), name = obj2ActName)
    val obj3Act = actorOf(Props(new ObjectActor(objectId = obj3ActName)), name = obj3ActName)

    Vector(obj1Act, obj2Act, obj3Act)

  }

  def receive = {
    case "start" =>
      log.info(s"Start $colour $name fleet")
      //TODO: create a fleet of ObjectActor from a list of objects with position (IMM.)
      fleetOpt = Some(createFleet())

    case "stop" =>
      log.info(s"Stop $colour $name fleet")
      stop(self)

    case "notify" =>
      fleetOpt match {
        case Some(fleet) =>
          for (obj <- fleet) {
            obj ! "notified..."
          }
          log.info(s"$colour $name fleet notified")
        case None =>
          log.info(s"$colour $name fleet not available not notified")
      }

    case "robin" =>
      //roundRobinRouter ! "robin"
      fleetOpt match {
        case Some(fleet) => fleet(2) ! "robin"
        case None =>
          log.info(s"$colour $name fleet not available not notified")
      }

    case message: String => log.info(s"FleetActor for $colour $name fleet received: $message")

    case _               => log.warning(s"FleetActor for $colour $name fleet received neither start nor stop nor any other string message !")

  }

}