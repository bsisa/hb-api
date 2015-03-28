package ch.bsisa.hyperbird.patman.simulations.controllers

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.ApiConfig
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import play.api._
import play.api.Logger
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import controllers.Assets
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.dao.ws.WSQueries
import securesocial.core.AuthenticationMethod
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import ch.bsisa.hyperbird.security.Role
import ch.bsisa.hyperbird.util.DateUtil
import java.util.Date
import play.libs.Akka
import akka.actor.Props
import ch.bsisa.hyperbird.patman.simulations.actors.SimulatorActor

/**
 * Patient management module simulation API controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
object SimulationApi extends Controller with securesocial.core.SecureSocial {

  
  /**
   * 
   */
  def simulate(dateFrom : String, dateTo : String) = SecuredAction(ajaxCall = true).async { request =>
    
    Logger.debug(s"SimulationApi.simulate: Test with parameters: ${dateFrom}, ${dateTo} called by user: ${request.user}")
    
    
    
    val simulatorActor = Akka.system.actorOf(Props(new SimulatorActor( DateUtil.hbDateFormat.parse(dateFrom), DateUtil.hbDateFormat.parse(dateTo) )), name = "simulatorActor")
    
    simulatorActor ! "Hello simulator..."
    
    simulatorActor ! "Stop"
    
  	scala.concurrent.Future(Ok(s"Test with parameters: ${dateFrom}, ${dateTo}"))

  }  
  
  
}