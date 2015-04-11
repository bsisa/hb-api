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

    case TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferReportActor notified of transfer from ${fromHospitalCode} to ${toHospitalCode} at ${fromSchedule}")
      //      val transferEvenElfin = ELFIN
      //      ElfinDAO.create(elfin)
      val futureTransferElfin: Future[ELFIN] = ElfinDAO.getNewFromCatalogue("TRANSFER")
      futureTransferElfin.map { elfinTransferTemplate =>

        try {

          //log.info(s">>>> OBTAINED TRANSFER FROM CATALOGUE:\n ${elfinTransferTemplate}")

          /**
           * Create at least one at most two TRANSFER entries:
           * 1) NATURE="add" for incomingSiBeds and typeScToSiBeds
           * 2) NATURE="remove" for type outgoingSiBeds
           * This to allow easy selection for reporting
           */

          // Compute list of beds to transfer (+)
          val bedsToAdd = incomingSiBeds ++ typeScToSiBeds

          if (bedsToAdd.size > 0) {
            val addTransferElfinFuture = HospitalHelper.buildTransferElfin(
              elfinTransferTemplate = elfinTransferTemplate, simulationId = simulationId, nature = Constants.TRANSFER_NATURE_ADD,
              fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, schedule = fromSchedule, beds = bedsToAdd)
            // Update database 
            addTransferElfinFuture.map { addTransferElfin =>
              ElfinDAO.create(addTransferElfin)
            }
          }

          // Compute list of beds to notify to remove (outgoing patients) (-)
          val bedsToRemove = outgoingSiBeds

          if (bedsToRemove.size > 0) {
            val removeTransferElfinFuture = HospitalHelper.buildTransferElfin(
              elfinTransferTemplate = elfinTransferTemplate, simulationId = simulationId, nature = Constants.TRANSFER_NATURE_REMOVE,
              fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, schedule = fromSchedule, beds = bedsToRemove)
            // Update database 
            removeTransferElfinFuture.map { removeTransferElfin =>
              ElfinDAO.create(removeTransferElfin)
            }
          }
          
          // TODO: we should also have beds to update for existing transfered beds which do have their 
          // transfer type changed or (to review) SI to SC patient type change 

        } catch {
          case e: Throwable => log.error(s"TransferReportActor complaining: ${e}")
        }

      }

    case TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferResponse id ${id} from ${fromHospitalCode} to ${toHospitalCode} status = ${status}")
    // We could write to database if status is not successful...

    case DataSetEmpty =>
      sender ! WorkCompleted("TransferReportActor")

  }



}