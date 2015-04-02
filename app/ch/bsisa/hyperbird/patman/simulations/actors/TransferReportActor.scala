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
            val addTransferElfinFuture = buildTransferElfin(
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
            val removeTransferElfinFuture = buildTransferElfin(
              elfinTransferTemplate = elfinTransferTemplate, simulationId = simulationId, nature = Constants.TRANSFER_NATURE_REMOVE,
              fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, schedule = fromSchedule, beds = bedsToRemove)
            // Update database 
            removeTransferElfinFuture.map { removeTransferElfin =>
              ElfinDAO.create(removeTransferElfin)
            }
          }

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

  def buildTransferElfin(elfinTransferTemplate: ELFIN, simulationId: String, nature: String, fromHospitalCode: String, toHospitalCode: String, schedule: Date, beds: List[Bed]): Future[ELFIN] = {

    val elfinTransferWithIdFuture: Future[ELFIN] = ElfinUtil.assignElfinId(elfinTransferTemplate)
    
    val elfinTransferWithBedsFuture = elfinTransferWithIdFuture.map { elfinTransferWithId =>
      val elfinTransferWithNewNatureGroupeSource = ElfinUtil.replaceElfinNatureGroupeSource(elfin = elfinTransferWithId, newNature = nature, newGroupe = elfinTransferWithId.GROUPE, newSource = Some(simulationId))
      val bedsHospitalWrapper = Hospital(code = fromHospitalCode, schedule = schedule, beds = beds)
      val bedsHospitalWrapperElfin = HospitalHelper.toElfin(bedsHospitalWrapper)

      val identifiantTransfer = IDENTIFIANT(AUT = Some("FluxPatients - Simulator"), NOM = None, ORIGINE = Option(fromHospitalCode), OBJECTIF = Option(toHospitalCode), DE = Option(DateUtil.getIsoDateFormatterWithoutTz.format(schedule)))
      val elfinTransferWithIdentifiant = ElfinUtil.replaceElfinIdentifiant(elfinTransferWithNewNatureGroupeSource, identifiantTransfer)
      val elfinTransferWithBeds = ElfinUtil.replaceElfinCaracteristiqueFractionL(elfinTransferWithIdentifiant, bedsHospitalWrapperElfin.CARACTERISTIQUE.get.FRACTION.get.L)
      elfinTransferWithBeds
    }
    
    elfinTransferWithBedsFuture
  }

}