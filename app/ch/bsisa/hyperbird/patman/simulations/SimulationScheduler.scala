package ch.bsisa.hyperbird.patman.simulations

import ch.bsisa.hyperbird.util.DateUtil
import java.util.Date
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import scala.concurrent.duration._
import akka.actor.Props
import akka.actor.Actor
import ch.bsisa.hyperbird.patman.simulations.messages.RefreshSimulationMessage
import ch.bsisa.hyperbird.dao.DbConfig
import ch.bsisa.hyperbird.patman.simulations.actors.RefreshSimulationActor
import play.api.libs.concurrent.Execution.Implicits._


/**
 * Patman Simulation scheduling procedure.
 *
 * Scheduling is optional as the simulation creation can be triggered from the API.
 * Although in practice it is more convenient to have simulation data automatically kept up to date.
 * This can be achieved with hb.patman.simulation.refresh-time configuration.
 *
 * @author Patrick Refondini
 */
object SimulationScheduler {

  def trySchedulingSimulation()(implicit patmanConfig: PatmanConfig, dbConfig: DbConfig) = {

    patmanConfig.simulationRefreshTime match {
      
      case Some(simulationRefreshTime) =>
        val hour = simulationRefreshTime._1.getOrElse(0)
        val minute = simulationRefreshTime._2.getOrElse(0)
        val second = simulationRefreshTime._3.getOrElse(0)
        val millisec = simulationRefreshTime._4.getOrElse(0)
        val secondsFromStart = DateUtil.firstExecutionFromNowInSeconds(hour, minute, second, millisec)
        val simulationRefreshActor = Akka.system.actorOf(Props(new RefreshSimulationActor), name = "refreshSimulationActor")

        Logger.info(s"Scheduling job in secondsFromStart = ${secondsFromStart} at ${new Date()}")

        val simulatedTransferCollectionResourceUrl = s"""${dbConfig.protocol}${dbConfig.hostName}:${dbConfig.port}${dbConfig.restPrefix}${dbConfig.databaseName}/${Constants.ELFIN_TRANSFER_SIMULATION_COLLECTION_ID}"""
        val simulatedHospitalStateCollectionResourceUrl = s"""${dbConfig.protocol}${dbConfig.hostName}:${dbConfig.port}${dbConfig.restPrefix}${dbConfig.databaseName}/${Constants.ELFIN_HOSPITAL_STATE_SIMULATION_COLLECTION_ID}"""
        val simulationCollectionResourceUrl = s"""${dbConfig.protocol}${dbConfig.hostName}:${dbConfig.port}${dbConfig.restPrefix}${dbConfig.databaseName}/${Constants.ELFIN_SIMULATION_COLLECTION_ID}"""

        val resourcesToDelUrls = List(simulationCollectionResourceUrl, simulatedTransferCollectionResourceUrl, simulatedHospitalStateCollectionResourceUrl)
        val msg = RefreshSimulationMessage(dbUser = dbConfig.userName, dbPassword = dbConfig.password, resourcesToDelete = resourcesToDelUrls)

        // 24 hours refresh rate for production
        val cl = Akka.system.scheduler.schedule(secondsFromStart.seconds, 24.hours, simulationRefreshActor, msg)
        
        // Use 3 minutes refresh rate for tests
        //Logger.warn(">>>> TEST SIMULATION REFRESH RATE ACTIVE : 3 minutes <<<<")
        //val cl = Akka.system.scheduler.schedule(secondsFromStart.seconds, 3.minutes, simulationRefreshActor, msg)
        
      case None => // nothing to do 
    }

  }

}