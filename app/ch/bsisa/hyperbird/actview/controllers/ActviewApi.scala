/**
 *
 */
package ch.bsisa.hyperbird.actview.controllers

import ch.bsisa.hyperbird.actview.actors.FleetActor
import controllers.Assets
import java.util.Date
import play.api.mvc.Controller
import play.api.libs.concurrent.Execution.Implicits._


//import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import akka.actor.Props


/**
 * @author Patrick Refondini
 *
 */
object ActviewApi  extends Controller with securesocial.core.SecureSocial {

   /**
   * Creates a new fleet with `name` and optional `colour` for display.
   */
  def launchFleet(name:String, colour:String) = SecuredAction(ajaxCall = true).async { request =>
    
    
    // Launch fleet
    val fleetActor = Akka.system.actorOf( Props(new FleetActor(name,colour)), s"fleet-$name" )
    fleetActor ! "start"
    
    val currentDate = new Date()
    val fleetLaunchedMsg = s"""
    {
      "fleet": {
            "name" : "$name",
            "status": "launched",
            "time": "${currentDate}"
        }
    }
    """
    scala.concurrent.Future(Ok(fleetLaunchedMsg).as(JSON))

  }
  
  
  def shutdownFleet(name:String) = SecuredAction(ajaxCall = true).async { request =>
    
    // Shutdown fleet
    Akka.system.actorSelection(s"fleet-$name") ! "stop"
    
    val currentDate = new Date()
    val fleetShutdownMsg = s"""
    {
      "fleet": {
            "name" : "$name",
            "status": "shutdown",
            "time": "${currentDate}"
        }
    }
    """
    scala.concurrent.Future(Ok(fleetShutdownMsg).as(JSON))

  }
  
}
