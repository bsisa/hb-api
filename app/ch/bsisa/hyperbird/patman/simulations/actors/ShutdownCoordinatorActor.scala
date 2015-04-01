package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.messages._
import play.api.libs.concurrent.Akka
import scala.concurrent.duration._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

class ShutdownCoordinatorActor extends Actor with ActorLogging {

  var shutdownSignals : List[String] = List()

  def receive = {

    case ShutdownSignal(message, terminationSize) => 
      log.info(s"Received ShutdownSignal(${message})")
      shutdownSignals = message :: shutdownSignals
      if (shutdownSignals.size >= terminationSize) {
        log.info(s"Obtained ${shutdownSignals.size} signals, requesting simulation to stop in 15 seconds...")
        Akka.system.scheduler.scheduleOnce(15 seconds, sender, StopSimulationRequest(s"ShudownCoordinator reached terminationSize ${terminationSize}"))
        //sender ! StopSimulationRequest(s"ShudownCoordinator reached terminationSize ${terminationSize}")
      } else {
        log.info(s"Waiting for ${terminationSize-shutdownSignals.size} signal${if ((terminationSize-shutdownSignals.size) > 1) "s" else ""}")
      }
  }

}
