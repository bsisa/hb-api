package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{Actor,ActorRef,ActorLogging}
import ch.bsisa.hyperbird.patman.simulations.messages.TransferRequest
import ch.bsisa.hyperbird.patman.simulations.messages.TransferResponse

class TransferActor(hospitalsActorRefs: Map[String, ActorRef]) extends Actor with ActorLogging {

  // Listen to request for transfer
  // Request hospital(s) to know whether the transfer is possible
  // Sends transfer approved / refused confirmation to requester
  // Perform transfer
  
    def receive = {
    case TransferRequest(id, incomingBeds, outgoingBeds, fromHospitalCode, toHospitalCode, message) => 
      log.info(s"Request for transfer from ${fromHospitalCode} to ${toHospitalCode}")
      hospitalsActorRefs(toHospitalCode) ! TransferRequest(id, incomingBeds, outgoingBeds, fromHospitalCode, toHospitalCode, message)
    case TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode , messagemessage) => 
      log.info(s"TransferResponse from ${fromHospitalCode} to ${toHospitalCode}")
  }
  
  
  
}