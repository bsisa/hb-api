package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import play.api.libs.concurrent.Execution.Implicits._
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.dao.ElfinDAO
import scala.concurrent.Future
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.util.ElfinUtil
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper

class TransferReportActor extends Actor with ActorLogging {

  def receive = {

    case TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferReportActor notified of transfer from ${fromHospitalCode} to ${toHospitalCode} at ${fromSchedule}")
      //      val transferEvenElfin = ELFIN
      //      ElfinDAO.create(elfin)
      val futureTransferElfin: Future[ELFIN] = ElfinDAO.getNewFromCatalogue("TRANSFER")
      futureTransferElfin.map { elfinTransferTemplate =>

        try {

          log.info(s">>>> OBTAINED TRANSFER FROM CATALOGUE:\n ${elfinTransferTemplate}")

          val elfinTransfer = ElfinUtil.assignElfinId(elfinTransferTemplate)
          val incomingAndtypeScToSiBeds = incomingSiBeds ++ typeScToSiBeds

          val incomingAndTypeScToSiHospitalWrapper = Hospital(code = fromHospitalCode, schedule = fromSchedule, beds = incomingAndtypeScToSiBeds)
          val incomingAndTypeScToSiHospitalWrapperElfin = HospitalHelper.toElfin(incomingAndTypeScToSiHospitalWrapper)

          val elfinTransferWithBeds = ElfinUtil.replaceElfinCaracteristiqueFractionL(elfinTransfer, incomingAndTypeScToSiHospitalWrapperElfin.CARACTERISTIQUE.get.FRACTION.get.L)

          val elfinTransferToCreate = elfinTransfer
          // Update database with new elfin
          ElfinDAO.create(elfinTransferToCreate)
        } catch {
          case e: Throwable => log.error(s"TransferReportActor complaining: ${e}")
          case z: Any => log.error(s"TransferReportActor complaining with Any caught: ${z}")
        }

      }

    case TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferResponse id ${id} from ${fromHospitalCode} to ${toHospitalCode} status = ${status}")
    // We could write to database if status is not successful...

    case DataSetEmpty =>
      sender ! WorkCompleted("TransferReportActor")

  }

}