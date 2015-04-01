package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import play.api.libs.concurrent.Execution.Implicits._

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.patman.simulations.messages.TransferRequest
import ch.bsisa.hyperbird.patman.simulations.messages.TransferResponse
import ch.bsisa.hyperbird.patman.simulations.messages.DataSetUpdateRequest
import ch.bsisa.hyperbird.patman.simulations.messages.DataSetUpdateResponse
import ch.bsisa.hyperbird.dao.ElfinDAO
import scala.concurrent.Future
import ch.bsisa.hyperbird.model.ELFIN

class TransferReportActor extends Actor with ActorLogging {

  def receive = {

    case TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferReportActor notified of transfer from ${fromHospitalCode} to ${toHospitalCode} at ${fromSchedule}")
      //      val transferEvenElfin = ELFIN
      //      ElfinDAO.create(elfin)
      val futureTransferElfin: Future[ELFIN] = ElfinDAO.getNewFromCatalogue("TRANSFER")
      futureTransferElfin.map { elfin =>

        log.info(">>>> OBTAINED TRANSFER FROM CATALOGUE:\n ${elfin}")

      }

    case TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferResponse id ${id} from ${fromHospitalCode} to ${toHospitalCode} status = ${status}")
    // We could write to database if status is not successful...

  }

}