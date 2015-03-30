package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.patman.simulations.messages.HospitalState
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.patman.simulations.model.Hospital

class HospitalActorCdf(name: String, bedsNb: Int) extends Actor with ActorLogging {

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

      // TODO: make use of it
      val patientTypeChange = HospitalHelper.getBedsWithPatientTypeChange(previousHospitalState, currentHospitalState)
      // patientTypeChangeFromScToSi should be transferred to PRT
      val patientTypeChangeFromScToSi = patientTypeChange._1
      // patientTypeChangeFromSiToSc should never be present here as SI patients move to PRT
      val patientTypeChangeFromSiToSc = patientTypeChange._2
      
      // Send SI movements as Transfer requests to PRT only if there some
      if (bedsWithIncomingPatientTypeSi != Nil || bedsWithOutgoingPatientTypeSi != Nil || patientTypeChangeFromScToSi != Nil) {
        transferActor ! TransferRequest(
          id = elfin.Id,
          incomingSiBeds = bedsWithIncomingPatientTypeSi,
          outgoingSiBeds = bedsWithOutgoingPatientTypeSi,
          typeScToSiBeds = patientTypeChangeFromScToSi, 
          fromHospitalCode = HOSPITAL_CODE_CDF,
          toHospitalCode = HOSPITAL_CODE_PRT,
          message = "Requesting incoming SI transfer")
      }

      //	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
      //	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
      //	    log.info(s"------------------------------ $name --------------------------------------")
      //	    log.info(s"$name> BedsWithIncomingPatient: " + HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState) )
      //	    log.info(s"$name> BedsWithOutgoingPatient: " + HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState) )
      //	    log.info(s"============================== $name - end   ==============================")

      // Check state received hospital id matches our name otherwise cancel simulation!
      sender ! NextHospitalStatesRequest(name)

    case TransferResponse(id, status, acceptedIncomingBeds, fromHospital, toHospital, message) => {
      // TODO: implement...

      status match {
        case TRANSFER_REQUEST_ACCEPTED => log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_ACCEPTED")
        case TRANSFER_REQUEST_REFUSED => log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_REFUSED")
        case TRANSFER_REQUEST_PARTIAL => log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_PARTIAL")
      }

    }
  }

}



