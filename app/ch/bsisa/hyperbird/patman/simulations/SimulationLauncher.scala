package ch.bsisa.hyperbird.patman.simulations

import scala.concurrent.Future
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor.Props
import ch.bsisa.hyperbird.patman.simulations.actors.SimulatorActor
import ch.bsisa.hyperbird.util.DateUtil

/**
 * Centralises simulation launch logic
 * 
 * @author Patrick Refondini
 */
object SimulationLauncher {

  def launchSimulation(author: String, dateFrom: String, dateTo: String): Future[String] = {

    val simulationIdFuture = HospitalHelper.createSimulationDatabaseEntry(Some(author), dateFrom, dateTo)

    simulationIdFuture.map { simulationId =>
      // Make use of ELFIN Id generator to obtain a `unique` identifier for the current simulation
      val simulatorActorName = s"simulatorActor_${simulationId}"

      // Create the simulation
      val simulatorActor = Akka.system.actorOf(
        Props(new SimulatorActor(simulationId, DateUtil.hbDateFormat.parse(dateFrom), DateUtil.hbDateFormat.parse(dateTo))),
        name = simulatorActorName)
      // Provide feedback with simulation identifier. 
      simulationId
    }

  }

}