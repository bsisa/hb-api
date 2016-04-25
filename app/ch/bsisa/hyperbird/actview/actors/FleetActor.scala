/**
 *
 */
package ch.bsisa.hyperbird.actview.actors

import akka.actor.{ Actor, ActorRef, ActorLogging, Props }
import akka.routing.BroadcastRouter
import akka.routing.RoundRobinRouter

import ch.bsisa.hyperbird.model._

import play.api.Play.current
import play.api.libs.concurrent.Akka
import scala.concurrent.Future

/**
 * @author Patrick Refondini
 *
 */
class FleetActor(name: String, colour: String) extends Actor with ActorLogging {

  import context._

  var fleetOpt: Option[Seq[ActorRef]] = None

  // TODO: load obj info from database 
  def createFleet(): Future[Seq[ActorRef]] = {

    // Hardcoded Test values
    val buildingColId = "G20040930101030005"
    val fountainColId = "G20040930101030004"
    val buildingClass = "IMMEUBLE"
    val fountainClass = "FONTAINE"    
    
    val fountainsConfig = (fountainColId, fountainClass)
    val buildingsConfig = (buildingColId, buildingClass)
    
    // Parameter
    val configPair = buildingsConfig

    val objColId = configPair._1
    val objClass = configPair._2

    /*
     * Example of produced XPath
     * "//ELFIN[@CLASSE='IMMEUBLE' and IDENTIFIANT/OBJECTIF='195']" // //ELFIN[@CLASSE='IMMEUBLE' and FORME/POINT[@FONCTION='BASE']]
     */
    val xPath = s"//ELFIN[@CLASSE='$objClass' and FORME/POINT[@FONCTION='BASE']]"
    
    
    // Make use of database API to obtain a set of objects with a start position 
    import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
    import ch.bsisa.hyperbird.dao.ws.WSQueries
    import ch.bsisa.hyperbird.Implicits._

    val elfinsFuture = XQueryWSHelper.queryElfins(WSQueries.filteredCollectionQuery(objColId, xPath))

    val objectsIdPositionFut: Future[Seq[(String, POINT)]] = elfinsFuture.map { elfins =>
      for (elfin <- elfins) yield ((elfin.Id, elfin.FORME.get.POINT(0)))
    }

    val actorRefSeqFut = objectsIdPositionFut.map { objectsIdPosition =>
      val actorRefSeq = for (objectIdPosition <- objectsIdPosition) yield {
        val objAct = actorOf(Props(new ObjectActor(objectId = objectIdPosition._1, startPosition = objectIdPosition._2)), name = objectIdPosition._1)
        objAct
      }
      actorRefSeq
    }
    actorRefSeqFut
  }

  def receive = {
    case "start" =>
      log.info(s"Start $colour $name fleet")
      createFleet().map { objActRefs =>
        fleetOpt = Some(objActRefs)
      }

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