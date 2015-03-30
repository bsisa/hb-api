package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.messages.TransferRequest
import ch.bsisa.hyperbird.patman.simulations.messages.TransferResponse
import ch.bsisa.hyperbird.patman.simulations.messages.DataSetUpdateRequest
import ch.bsisa.hyperbird.patman.simulations.messages.DataSetUpdateResponse

class TransferActor(hospitalsActorRefs: Map[String, ActorRef], dataSetActorRef : ActorRef) extends Actor with ActorLogging {

  // Listen to request for transfer
  // Request hospital(s) to know whether the transfer is possible
  // Sends transfer approved / refused confirmation to requester
  // Perform transfer

  def receive = {

    case TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"Request for transfer from ${fromHospitalCode} to ${toHospitalCode}")
      hospitalsActorRefs(toHospitalCode) ! TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message)

    case TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(s"TransferResponse from ${fromHospitalCode} to ${toHospitalCode}")
      // Forward response to original requester (fromHospitalCode)
      hospitalsActorRefs(fromHospitalCode) ! TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode, fromSchedule, message)
      
    case DataSetUpdateRequest(allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule) => 
      dataSetActorRef ! DataSetUpdateRequest(allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule)
      
    case DataSetUpdateResponse(status, fromHospitalCode, toHospitalCode, fromSchedule) => 
      hospitalsActorRefs(fromHospitalCode) ! DataSetUpdateResponse(status, fromHospitalCode, toHospitalCode, fromSchedule)
  }

}