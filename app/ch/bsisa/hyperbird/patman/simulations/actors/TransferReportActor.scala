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

class TransferReportActor(simulationId: String) extends Actor with ActorLogging {

  def receive = {

    case TransferRequestCreate(id, bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferReportActor received: ${message}")
      val futureTransferElfin: Future[ELFIN] = ElfinDAO.getNewFromCatalogue("TRANSFER")
      futureTransferElfin.map { elfinTransferTemplate =>
        try {
          val bedsToAdd = bedsWithIncomingPatientTypeSi ++ patientTypeChangeFromScToSi
          if (bedsToAdd != Nil && bedsToAdd.size > 0) {
            val addTransferElfinFuture = HospitalHelper.buildTransferElfin(
              elfinTransferTemplate = elfinTransferTemplate, simulationId = simulationId, nature = Constants.TRANSFER_NATURE_ADD,
              fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, schedule = fromSchedule, beds = bedsToAdd)
            // Update database 
            addTransferElfinFuture.map { addTransferElfin =>
              ElfinDAO.create(addTransferElfin)
            }
          }
        } catch {
          case e: Throwable => log.error(s"TransferReportActor complaining: ${e}")
        }
      }

    case TransferRequestUpdate(id, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferReportActor received: ${message}")
      val futureTransferElfin: Future[ELFIN] = ElfinDAO.getNewFromCatalogue("TRANSFER")
      futureTransferElfin.map { elfinTransferTemplate =>
        try {
          val bedsToUpdate = patientTypeChangeFromSiToSc ++ bedsWithTransferTypeOnlyChangePatientTypeSi
          if (bedsToUpdate != Nil && bedsToUpdate.size > 0) {
            val updateTransferElfinFuture = HospitalHelper.buildTransferElfin(
              elfinTransferTemplate = elfinTransferTemplate, simulationId = simulationId, nature = Constants.TRANSFER_NATURE_UPDATE,
              fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, schedule = fromSchedule, beds = bedsToUpdate)
            // Update database 
            updateTransferElfinFuture.map { updateTransferElfin =>
              ElfinDAO.create(updateTransferElfin)
            }
          }
        } catch {
          case e: Throwable => log.error(s"TransferReportActor complaining: ${e}")
        }
      }
      
    case TransferRequestDelete(id, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferReportActor received: ${message}")
      val futureTransferElfin: Future[ELFIN] = ElfinDAO.getNewFromCatalogue("TRANSFER")
      futureTransferElfin.map { elfinTransferTemplate =>
        try {
          val bedsToDelete = bedsWithOutgoingPatientTypeSi ++ bedsWithOutgoingPatientTypeSc
          if (bedsToDelete != Nil && bedsToDelete.size > 0) {
            val deleteTransferElfinFuture = HospitalHelper.buildTransferElfin(
              elfinTransferTemplate = elfinTransferTemplate, simulationId = simulationId, nature = Constants.TRANSFER_NATURE_REMOVE,
              fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, schedule = fromSchedule, beds = bedsToDelete)
            // Update database 
            deleteTransferElfinFuture.map { deleteTransferElfin =>
              ElfinDAO.create(deleteTransferElfin)
            }
          }
        } catch {
          case e: Throwable => log.error(s"TransferReportActor complaining: ${e}")
        }
      }

    case DataSetEmpty =>
      sender ! WorkCompleted("TransferReportActor")

  }

}