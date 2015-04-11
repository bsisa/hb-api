package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.messages._

class TransferActor(hospitalsActorRefs: Map[String, ActorRef], transferReportActorRef : ActorRef) extends Actor with ActorLogging {

  // Listen to request for transfer
  // Request hospital(s) to know whether the transfer is possible
  // Sends transfer approved / refused confirmation to requester
  // Perform transfer

  def receive = {

    case TransferRequestCreate(id, bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      hospitalsActorRefs(toHospitalCode) ! TransferRequestCreate(id, bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi, fromHospitalCode, toHospitalCode, fromSchedule, message)
      transferReportActorRef ! TransferRequestCreate(id, bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi, fromHospitalCode, toHospitalCode, fromSchedule, message)
      
    case TransferRequestUpdate(id, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      hospitalsActorRefs(toHospitalCode) ! TransferRequestUpdate(id, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, fromHospitalCode, toHospitalCode, fromSchedule, message)
      transferReportActorRef ! TransferRequestUpdate(id, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, fromHospitalCode, toHospitalCode, fromSchedule, message)
      
    case TransferRequestDelete(id, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      hospitalsActorRefs(toHospitalCode) ! TransferRequestDelete(id, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message)
      transferReportActorRef ! TransferRequestDelete(id, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message)      
      
      
    case TransferResponseCreate(correlationId, status, fromHospitalCode, toHospitalCode, message) => 
      log.info(message)
      hospitalsActorRefs(fromHospitalCode) ! TransferResponseCreate(correlationId, status, fromHospitalCode, toHospitalCode, message)
      //transferReportActorRef ! TransferResponseCreate(correlationId, status, fromHospitalCode, toHospitalCode, message)      
      
    case TransferResponseUpdate(correlationId, status, fromHospitalCode, toHospitalCode, message) => 
      log.info(message)
      hospitalsActorRefs(fromHospitalCode) ! TransferResponseUpdate(correlationId, status, fromHospitalCode, toHospitalCode, message)
      //transferReportActorRef ! TransferResponseUpdate(correlationId, status, fromHospitalCode, toHospitalCode, message)            
      
    case TransferResponseDelete(correlationId, status, fromHospitalCode, toHospitalCode, message) => 
      log.info(message)
      hospitalsActorRefs(fromHospitalCode) ! TransferResponseDelete(correlationId, status, fromHospitalCode, toHospitalCode, message)
      //transferReportActorRef ! TransferResponseDelete(correlationId, status, fromHospitalCode, toHospitalCode, message)
      
      
//    case TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
//      //log.info(s"Request for transfer from ${fromHospitalCode} to ${toHospitalCode}")
//      hospitalsActorRefs(toHospitalCode) ! TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message)
//      transferReportActorRef ! TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message)
//
//    case TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
//      //log.info(s"TransferResponse from ${fromHospitalCode} to ${toHospitalCode}")
//      // Forward response to original requester (fromHospitalCode)
//      hospitalsActorRefs(fromHospitalCode) ! TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode, fromSchedule, message)
//      transferReportActorRef ! TransferResponse(id, status, acceptedIncomingBeds, fromHospitalCode, toHospitalCode, fromSchedule, message)
    
    case DataSetEmpty => 
      sender ! WorkCompleted("TransferActor")
      
//    case DataSetUpdateRequest(id, allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule) => 
//      dataSetActorRef ! DataSetUpdateRequest(id, allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule)
//      
//    case DataSetUpdateResponse(id, status, allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule) => 
//      hospitalsActorRefs(fromHospitalCode) ! DataSetUpdateResponse(id, status, allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule)
//      hospitalsActorRefs(toHospitalCode) ! DataSetUpdateResponse(id, status, allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule)
  }

}
