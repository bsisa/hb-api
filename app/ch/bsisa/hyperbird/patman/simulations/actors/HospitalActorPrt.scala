package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.patman.simulations.model.Hospital

/**
 * Models PRT Hospital intensive care data as bed, patient, patient type, transfer type 
 * and related behaviour intended for simulation.
 */
class HospitalActorPrt(name: String, bedsNb: Int) extends Actor with ActorLogging {

  var previousHospitalState: Option[Hospital] = None
  var currentHospitalState: Option[Hospital] = None

  def receive = {
    case HospitalState(elfin, transferActor) =>
      log.info(s"$name> HospitalActor(${name}) received new hospitalState schedule ${elfin.IDENTIFIANT.get.DE.get}")
      val hospital = HospitalHelper.toHospital(elfin)
      //	    log.info(s"============================== $name - start ==============================")
      //	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
      //	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
      //	    log.info(s"------------------------------ $name --------------------------------------")

      // Roll states
      previousHospitalState = currentHospitalState
      currentHospitalState = Some(hospital)

      // Check incoming / outgoing patients
      //	    val incoming = HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState)
      //	    val bedsWithIncomingPatientTypeSi = incoming._1
      //	    val bedsWithIncomingPatientTypeSc = incoming._2
      //	    
      //	    val outgoing = HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState)
      //	    val bedsWithOutgoingPatientTypeSi = outgoing._1
      //	    val bedsWithOutgoingPatientTypeSc = outgoing._2

      //	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
      //	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
      //	    log.info(s"------------------------------ $name --------------------------------------")
      //	    log.info(s"$name> BedsWithIncomingPatient: " + HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState) )
      //	    log.info(s"$name> BedsWithOutgoingPatient: " + HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState) )
      //	    log.info(s"============================== $name - end   ==============================")

      // Send SI movements as Transfer requests to PRT
      //        transferActor ! TransferRequest(id = elfin.Id , incomingBeds = bedsWithIncomingPatientTypeSi, outgoingBeds = bedsWithIncomingPatientTypeSi , fromHospitalCode = HOSPITAL_CODE_CDF, toHospitalCode = HOSPITAL_CODE_PRT, message = "Requesting incoming SI transfer")

      // Check state received hospital id matches our name otherwise cancel simulation!
      sender ! NextHospitalStatesRequest(name)

    case TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.info(message)
      //log.info(s"Request for transfer from ${fromHospitalCode} to ${toHospitalCode} with in = ${incomingSiBeds.size}, out = ${outgoingSiBeds.size}, typeScToSiBeds = ${typeScToSiBeds.size} DELTA = ${incomingSiBeds.size - outgoingSiBeds.size + typeScToSiBeds.size}")
      // OBSOLETE TODO: Request DataSet update for new Si beds => incomingSiBeds + typeScToSiBeds
      val allTransferredSiBeds = incomingSiBeds ::: typeScToSiBeds
      //sender ! DataSetUpdateRequest(id, allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule)

      // =========================================================================================================
      // TODO: Notify TransferReportActor of transfer events: Hospital from => to, Schedule, PatientNb, PatientType, TransferType, Reason for transfer: New SI or SC to SI

      // TODO: Update current PRT hospital state with new SI beds (outgoingSiBeds should be made OBSOLETE and always be 0 (Once DataSet update is implemented))

      // =========================================================================================================          
      // TODO: Confirm TRANSFER_REQUEST_ACCEPTED with TransferResponse to TransferActor
      sender ! TransferResponse(id, TRANSFER_REQUEST_ACCEPTED, allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, "Transfer report notification (TODO) and HospitalActor PRT update (TODO) succeeded.")
      // Request next data.
      context.parent ! NextHospitalStatesRequest(name)      
      
    case TransferResponse(id, status, acceptedIncomingBeds, fromHospital, toHospital, fromSchedule, message) => {
      // No transfer request from PRT at the moment
      log.warning(s"Unexpected transferResponse at PRT: id = $id, $message")
    }

  }

}



