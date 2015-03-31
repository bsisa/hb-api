package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.patman.simulations.messages.HospitalState
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.patman.simulations.model.Hospital

/**
 * Models CDF Hospital intensive care data such as bed, patient, patient type, transfer type
 * and related behaviour intended for simulation.
 */
class HospitalActorCdf(name: String, bedsNb: Int) extends Actor with ActorLogging {

  /**
   * Static state representation reflecting HOSPITAL_STATE database entries
   * at a given time or schedule (08:00, 16:00, 22:00)
   */
  var previousHospitalState: Option[Hospital] = None
  var currentHospitalState: Option[Hospital] = None

  /**
   * Dynamic state representation build from HOSPITAL_STATE database entries
   * change events from schedule to schedule (08:00, 16:00, 22:00)
   */
  var simulatedHospitalState: Option[Hospital] = None

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
      val incoming = HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState)
      // bedsWithIncomingPatientTypeSi should be transferred to PRT
      val bedsWithIncomingPatientTypeSi = incoming._1
      // bedsWithIncomingPatientTypeSc should stay at CDF
      val bedsWithIncomingPatientTypeSc = incoming._2

      val outgoing = HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState)
      // bedsWithOutgoingPatientTypeSi should not happen at CDF as SI patient are moved to PRT
      val bedsWithOutgoingPatientTypeSi = outgoing._1
      // bedsWithOutgoingPatientTypeSc are expected at CDF, we do nothing with it at the moment
      val bedsWithOutgoingPatientTypeSc = outgoing._2

      // SC to SI type change need to be transferred.
      val patientTypeChange = HospitalHelper.getBedsWithPatientTypeChange(previousHospitalState, currentHospitalState)
      // patientTypeChangeFromScToSi should be transferred to PRT
      val patientTypeChangeFromScToSi = patientTypeChange._1
      // patientTypeChangeFromSiToSc should never be present here as SI patients move to PRT
      val patientTypeChangeFromSiToSc = patientTypeChange._2

      val tranferTypeOnlyChange = HospitalHelper.getBedsWithTransfertTypeChangeOnly(previousHospitalState, currentHospitalState)

      // Update current CDT simulatedHospitalState removing transfered SI beds
      simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForCdf(
        currentSimulatedHospitalStateOption = simulatedHospitalState,
        newStaticHospitalStateOption = currentHospitalState,
        bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
        bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, patientTypeChangeFromScToSi,
        patientTypeChangeFromSiToSc, tranferTypeOnlyChange)

        log.info(s"${name}> SIMULATED HS: ${simulatedHospitalState}")
        
      // Send SI movements as Transfer requests to PRT only if necessary
      if (bedsWithIncomingPatientTypeSi != Nil || bedsWithOutgoingPatientTypeSi != Nil || patientTypeChangeFromScToSi != Nil) {
        transferActor ! TransferRequest(
          id = elfin.Id,
          incomingSiBeds = bedsWithIncomingPatientTypeSi,
          outgoingSiBeds = bedsWithOutgoingPatientTypeSi,
          typeScToSiBeds = patientTypeChangeFromScToSi,
          fromHospitalCode = HOSPITAL_CODE_CDF,
          toHospitalCode = HOSPITAL_CODE_PRT,
          fromSchedule = hospital.schedule,
          message = s"Requesting SI transfer for ${hospital.schedule} from ${HOSPITAL_CODE_CDF} to ${HOSPITAL_CODE_PRT} with:\n+ ${bedsWithIncomingPatientTypeSi.size} in, - ${bedsWithOutgoingPatientTypeSi.size} out, + ${patientTypeChangeFromScToSi.size} SC to SI in")
      } else {
        // No transfer response to wait for, request next data.
        sender ! NextHospitalStatesRequest(name)
      }

    //	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
    //	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
    //	    log.info(s"------------------------------ $name --------------------------------------")
    //	    log.info(s"$name> BedsWithIncomingPatient: " + HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState) )
    //	    log.info(s"$name> BedsWithOutgoingPatient: " + HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState) )
    //	    log.info(s"============================== $name - end   ==============================")

    case TransferResponse(id, status, acceptedIncomingBeds, fromHospital, toHospital, fromSchedule, message) => {
      status match {
        case TRANSFER_REQUEST_ACCEPTED =>
          log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_ACCEPTED, requesting next data.")
          // Request next data.
          context.parent ! NextHospitalStatesRequest(name)
        case TRANSFER_REQUEST_REFUSED =>
          // We should not obtain this
          log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_REFUSED")
          context.parent ! StopSimulationRequest(s"TransferRequest id = $id : TRANSFER_REQUEST_REFUSED")
        case TRANSFER_REQUEST_PARTIAL =>
          // We should not obtain this
          log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_PARTIAL")
          context.parent ! StopSimulationRequest(s"TransferRequest id = $id : TRANSFER_REQUEST_PARTIAL")
      }

    }
  }

}



