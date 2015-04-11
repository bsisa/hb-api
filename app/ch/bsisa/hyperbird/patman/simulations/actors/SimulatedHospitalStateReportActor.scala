package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import play.api.libs.concurrent.Execution.Implicits._
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.dao.ElfinDAO
import scala.concurrent.Future
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.util.ElfinUtil
import ch.bsisa.hyperbird.patman.simulations.Constants
import ch.bsisa.hyperbird.patman.simulations.model.Bed
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.model.IDENTIFIANT
import ch.bsisa.hyperbird.util.DateUtil
import java.util.Date

class SimulatedHospitalStateReportActor(simulationId: String) extends Actor with ActorLogging {

  def receive = {

    case SimulatedHospitalState(hospitalState) =>
      log.info(s"SimulatedHospitalState notified for ${hospitalState.code}  at ${hospitalState.schedule}")

      val futureHospitalStateElfin: Future[ELFIN] = ElfinDAO.getNewFromCatalogue("HOSPITAL_STATE")
      futureHospitalStateElfin.map { elfinHospitalStateTemplate =>

        try {

          val elfinHospitalStateFuture = HospitalHelper.buildHospitalStateElfin(
            elfinHospitalStateTemplate = elfinHospitalStateTemplate,
            simulationId = simulationId,
            hospitalState = hospitalState)
          // Update database 
          elfinHospitalStateFuture.map { elfinHospitalState =>
            ElfinDAO.create(elfinHospitalState)
          }

        } catch {
          case e: Throwable => log.error(s"SimulatedHospitalStateReportActor complaining: ${e}")
        }

      }

    case DataSetEmpty =>
      sender ! WorkCompleted("TransferReportActor")

  }

}