/**
 *
 */
package ch.bsisa.hyperbird.actview.actors

import akka.actor.{ Actor, ActorLogging }
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor.Props

/**
 * @author Patrick Refondini
 *
 */
class FleetActor(name:String, colour:String) extends Actor with ActorLogging {

  
  def receive = {
    case "start" => 
      log.info(s"Start $colour $name fleet")
      //TODO: create a fleet of ObjectActor from a list of objects with position (IMM.)
      val obj1ActName = s"${name}-1"
      val obj2ActName = s"${name}-2"
      val obj3ActName = s"${name}-3"
      val obj1Act = Akka.system.actorOf(Props(new ObjectActor(objectId = obj1ActName)), name = obj1ActName )
      val obj2Act = Akka.system.actorOf(Props(new ObjectActor(objectId = obj2ActName)), name = obj2ActName )
      val obj3Act = Akka.system.actorOf(Props(new ObjectActor(objectId = obj3ActName)), name = obj3ActName )
      
    case "stop"  => 
      log.info(s"Stop $colour $name fleet")
      context.stop(self)
    
    case _ => log.warning("FleetActor for $colour $name fleet received neither start nor stop string message !")
    
  }
  
  
}