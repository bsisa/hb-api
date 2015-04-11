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
import ch.bsisa.hyperbird.util.ElfinIdGenerator
import scala.concurrent.Future
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper

/**
 * Patient management module simulation API controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
object SimulationApi extends Controller with securesocial.core.SecureSocial {

  /**
   * Starts a new simulation with the provided parameters
   */
  def simulate(dateFrom: String, dateTo: String) = SecuredAction(ajaxCall = true).async { request =>

    val author: String = request.user.identityId.userId

    val simulationIdFuture = HospitalHelper.createSimulationDatabaseEntry(Some(author), dateFrom, dateTo)

    simulationIdFuture.map { simulationId =>
      // Make use of ELFIN Id generator to obtain a `unique` identifier for the current simulation
      val simulatorActorName = s"simulatorActor_${simulationId}"
      Logger.debug(s"SimulationApi.simulate: Test with parameters: ${dateFrom}, ${dateTo} called by user: ${request.user}")
      // Create the simulation
      val simulatorActor = Akka.system.actorOf(Props(new SimulatorActor(simulationId, DateUtil.hbDateFormat.parse(dateFrom), DateUtil.hbDateFormat.parse(dateTo))), name = simulatorActorName)
      // Provide feedback with simulation identifier. 
      // TODO: possibly deal with exceptions.
      Ok(s"Simulator id = ${simulationId}, name = ${simulatorActorName} with parameters: ${dateFrom}, ${dateTo}")
    }

  }

}