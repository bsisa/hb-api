package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.messages._

/**
 * Forwards transfer create, update, delete requests to hospital destination and to transfer report actor.
 * Forwards transfer create, update, delete responses back to hospital source
 */
class TransferActor(hospitalsActorRefs: Map[String, ActorRef], transferReportActorRef : ActorRef) extends Actor with ActorLogging {


  def receive = {

    case TransferRequestCreate(id, bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      val tReqCreate = TransferRequestCreate(id, bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi, fromHospitalCode, toHospitalCode, fromSchedule, message)
      hospitalsActorRefs(toHospitalCode) ! tReqCreate 
      transferReportActorRef ! tReqCreate
      
    case TransferRequestUpdate(id, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      val tReqUpdate = TransferRequestUpdate(id, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message)
      hospitalsActorRefs(toHospitalCode) ! tReqUpdate
      transferReportActorRef ! tReqUpdate
      
    case TransferRequestDelete(id, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      val tReqDelete = TransferRequestDelete(id, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message)
        hospitalsActorRefs(toHospitalCode) ! tReqDelete
      transferReportActorRef ! tReqDelete      
      
      
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


    case DataSetEmpty => 
      sender ! WorkCompleted("TransferActor")

  }

}
