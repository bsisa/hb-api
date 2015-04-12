package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.messages._
import play.api.libs.concurrent.Akka
import scala.concurrent.duration._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import ch.bsisa.hyperbird.patman.simulations.model.HospitalSimulationSummary
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper

class ShutdownCoordinatorActor(simulationId:String) extends Actor with ActorLogging {

  var shutdownSignals : List[String] = List()
  var hospitalSummaryEntries : List[HospitalSimulationSummary] = List()
  def receive = {

    case ShutdownSignal(message, terminationSize, hssOpt) => 
      
      log.info(s"Received ShutdownSignal(${message})")
      
      // Store hss if present
      hssOpt match {
        case Some(hss) => 
          hospitalSummaryEntries = hss :: hospitalSummaryEntries
          log.info("Added hospital summary entry.")
        case None => 
          log.info("No hospital summary entry.")
      }        
      
      // Store signal
      shutdownSignals = message :: shutdownSignals

      if (shutdownSignals.size >= terminationSize) {
        
    	log.info("Updating SIMULATION with hospital summary entries")
        HospitalHelper.updateSimulationDatabaseEntry(simulationId = simulationId, hssList = hospitalSummaryEntries)
        
        log.info(s"Obtained ${shutdownSignals.size} signals, requesting simulation to stop in 15 seconds...")
        
        
        
        Akka.system.scheduler.scheduleOnce(15 seconds, sender, StopSimulationRequest(s"ShudownCoordinator reached terminationSize ${terminationSize}"))
        //sender ! StopSimulationRequest(s"ShudownCoordinator reached terminationSize ${terminationSize}")
      } else {
        log.info(s"Waiting for ${terminationSize-shutdownSignals.size} signal${if ((terminationSize-shutdownSignals.size) > 1) "s" else ""}")
      }
  }

}
