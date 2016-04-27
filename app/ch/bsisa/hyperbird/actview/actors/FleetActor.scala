package ch.bsisa.hyperbird.actview.actors

import akka.actor.{ Actor, ActorRef, ActorLogging, ActorSelection, Props }
import akka.routing.BroadcastRouter
import akka.routing.RoundRobinRouter

import ch.bsisa.hyperbird.actview.LoadFleet
import ch.bsisa.hyperbird.actview.DeleteFleet
import ch.bsisa.hyperbird.actview.BroadcastFleet
import ch.bsisa.hyperbird.actview.BroadcastFleetDestination
import ch.bsisa.hyperbird.actview.SetDestination

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.util.ElfinUtil

import play.api.Play.current
import play.api.libs.concurrent.Akka
import scala.concurrent.Future

/**
 * 
 * `FleetActor` models a fleet of objects themself modelled as `ObjectActor`
 * 
 * @author Patrick Refondini
 *
 */
class FleetActor(name: String, colour: String) extends Actor with ActorLogging {

  import context._

  /**
   * Sequence of references to fleet objects. 
   * This mutable Option reference is only set once upon LoadFleet event.
   */
  var fleetOpt: Option[Seq[ActorRef]] = None

  /**
   * Process fleet events
   */
  def receive = {

    case LoadFleet(objCollection, objClass) =>
      fleetOpt match {
        case Some(fleet) =>
          log.info(s"$colour $name fleet with $objClass already loaded. No further load action performed.")
        case None =>
          log.info(s"Start $colour $name fleet with $objClass")
          createFleet(name, objCollection, objClass).map { objActRefs =>
            fleetOpt = Some(objActRefs)
          }
      }
      
    case DeleteFleet =>
      log.info(s"Stop $colour $name fleet")
      stop(self)
      
    case BroadcastFleet(message) =>
      fleetOpt match {
        case Some(fleet) =>
          for (obj <- fleet) {
            obj ! message
          }
          log.info(s"$colour $name fleet notified")
        case None =>
          log.info(s"$colour $name fleet not available not notified")
      }
      
    case BroadcastFleetDestination(destination) => 
        fleetOpt match {
        case Some(fleet) =>
          for (obj <- fleet) {
            obj ! SetDestination(destination)
          }
          log.info(s"$colour $name fleet notified")
        case None =>
          log.info(s"$colour $name fleet not available not notified")
      }

    case message: String =>
      log.info(s"FleetActor for $colour $name fleet received: $message")

    case _ =>
      log.warning(s"FleetActor for $colour $name fleet received neither start nor stop nor any other string message !")

  }


  /**
   * Loads a fleet of object info from database and creates corresponding ObjectActor.
   */
  def createFleet(fleetName: String, objCollection: String, objClass: String): Future[Seq[ActorRef]] = {
    /*
     * Example of produced XPath
     * "//ELFIN[@CLASSE='IMMEUBLE' and IDENTIFIANT/OBJECTIF='195']" // //ELFIN[@CLASSE='IMMEUBLE' and FORME/POINT[@FONCTION='BASE']]
     */
    val xPath = s"//ELFIN[@CLASSE='$objClass' and FORME/POINT[@FONCTION='BASE']]"

    // Make use of database API to obtain a set of objects with a start position 
    import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
    import ch.bsisa.hyperbird.dao.ws.WSQueries
    import ch.bsisa.hyperbird.Implicits._

    val elfinsFuture = XQueryWSHelper.queryElfins(WSQueries.filteredCollectionQuery(objCollection, xPath))

    val objectsIdPositionFut: Future[Seq[(String, POINT, ELFIN)]] = elfinsFuture.map { elfins =>
      for (elfin <- elfins) yield ((elfin.Id, elfin.FORME.get.POINT(0), ElfinUtil.getElfinForMap(elfin)))
    }

    val actorRefSeqFut = objectsIdPositionFut.map { objectsIdPosition =>
      val actorRefSeq = for (objectIdPosition <- objectsIdPosition) yield {
        val objAct = actorOf(Props(new ObjectActor(objectId = objectIdPosition._1, fleetName = fleetName, startPosition = objectIdPosition._2, elfin = objectIdPosition._3)), name = objectIdPosition._1)
        objAct
      }
      actorRefSeq
    }
    actorRefSeqFut
  }  
  
 
  
}
