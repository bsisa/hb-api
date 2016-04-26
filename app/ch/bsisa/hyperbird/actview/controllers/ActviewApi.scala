/**
 *
 */
package ch.bsisa.hyperbird.actview.controllers

import akka.ConfigurationException
import akka.actor.ActorSelection
import akka.actor.InvalidActorNameException
import akka.actor.Props

import ch.bsisa.hyperbird.actview.actors.FleetActor
import ch.bsisa.hyperbird.actview.ActviewMessage
import ch.bsisa.hyperbird.actview.BroadcastFleet
import ch.bsisa.hyperbird.actview.DeleteFleet
import ch.bsisa.hyperbird.actview.LoadFleet
import ch.bsisa.hyperbird.actview.GetDestination
import ch.bsisa.hyperbird.actview.GetPosition
import controllers.Assets
import java.util.Date
import play.api.mvc.Controller
import play.api.libs.concurrent.Execution.Implicits._

import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka

import scala.concurrent.Future

/**
 * @author Patrick Refondini
 *
 */
object ActviewApi extends Controller with securesocial.core.SecureSocial {

  // Make use of Play actor system
  val actorSystem = Akka.system

  /**
   * Returns a fleet reference by fleet `name`
   */
  def getFleetSelection(name: String): ActorSelection = {
    val feetSelectionPath = s"/user/fleet-$name"
    Logger.info(s"feetSelectionPath = ${feetSelectionPath}")
    actorSystem.actorSelection(feetSelectionPath)
  }
  
  /**
   * Returns a fleet reference by fleet `name`
   */
  def getObjectSelection(fleetName: String, objectId : String): ActorSelection = {
    val objectSelectionPath = s"/user/fleet-$fleetName/$objectId"
    Logger.info(s"objectSelectionPath = ${objectSelectionPath}")
    actorSystem.actorSelection(objectSelectionPath)
  }  

  /**
   * Returns a server notification actor reference 
   */
  def getServerNotification(): ActorSelection = {
    val serverSideNotificationPath = s"/user/serverSideNotificationActor"
    actorSystem.actorSelection(serverSideNotificationPath)
  }  
  
  
  /**
   * Returns a fleet json string message filled with provided `fleetName` and `statusMessage` informations.
   */
  def getJsonFleetMessage(fleetName: String, statusMessage: String) = s"""
    {
      "fleet": {
            "name" : "$fleetName",
            "status": "$statusMessage",
            "time": "${new Date()}"
        }
    }
    """

  /**
   * Creates a new fleet with `name` and optional `colour` for display.
   */
  def launchFleet(name: String, colour: String) = SecuredAction(ajaxCall = true).async { request =>

    val currentDate = new Date()

    // Hard coded Test values
    val fountainsConfig = ("G20040930101030004", "FONTAINE" )
    val buildingsConfig = ("G20040930101030005", "IMMEUBLE")
    
    // Launch fleet
    val fleetLaunchedStatus = try {

      // Create fleet actor
      val fleetActor = actorSystem.actorOf(Props(new FleetActor(name, colour)), s"fleet-$name")
      // Initialize fleet actor
      fleetActor ! LoadFleet(objCollection = buildingsConfig._1, objClass = buildingsConfig._2)
      //fleetActor ! LoadFleet(objCollection = fountainsConfig._1, objClass = fountainsConfig._2)
      s"SUCCESS: ${name} fleet has been launched at path: ${fleetActor.path}"

    } catch {
      case e: ConfigurationException    => s"FAILURE: ${name} fleet could not be launched a configuration took place: ${e}"
      case e: InvalidActorNameException => s"FAILURE: ${name} fleet could not be launched: ${e.message}"
      case e: Throwable                 => s"FAILURE: ${name} fleet could not be launched due to unexpected exception: ${e}"
    }

    val fleetLaunchedMsg = getJsonFleetMessage(name, fleetLaunchedStatus)

    // Note: Use of Ok even in case of failure is intended for current POC design.
    Future(Ok(fleetLaunchedMsg).as(JSON))

  }

  /**
   * Shuts down a fleet by `name`.
   */  
  def shutdownFleet(fleetName: String) = SecuredAction(ajaxCall = true).async { request =>

    // Shutdown fleet
    val fleetSelection = getFleetSelection(fleetName)
    // TODO: make Stop message a case obj message
    fleetSelection ! DeleteFleet

    val fleetShutdownMsg = getJsonFleetMessage(fleetName, "shutdown")
    Future(Ok(fleetShutdownMsg).as(JSON))

  }

  /**
   * Creates a new fleet with `name` and optional `colour` for display.
   */
  def broadcastFleet(fleetName: String, message: String) = SecuredAction(ajaxCall = true).async { request =>

    val fleet = getFleetSelection(fleetName)
    //fleet.router
    fleet ! BroadcastFleet(message)
    Future(Ok)

  }
  
  /**
   * Creates a new fleet with `name` and optional `colour` for display.
   */
  def getObjDestination(fleetName: String, objectId: String) = SecuredAction(ajaxCall = true).async { request =>

    val objectRef = getObjectSelection(fleetName, objectId)
    Logger.info("getObjDestination sending message: objectRef ! GetDestination")
    objectRef ! GetDestination
    Future(Ok)

  }  

  
  /**
   * Creates a new fleet with `name` and optional `colour` for display.
   */
  def getObjPosition(fleetName: String, objectId: String) = SecuredAction(ajaxCall = true).async { request =>

    val objectRef = getObjectSelection(fleetName, objectId)
    Logger.info("getObjPosition sending message: objectRef ! GetPosition")
    objectRef ! GetPosition
    Future(Ok)

  }  
  
}
