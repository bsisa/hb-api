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
   * Messages stack lifecycle
   * 
   * 1) Empty
   * 2) Filled in any order with: 
   *    - HospitalState
   *    - TransferRequestCreate
   *    - TransferRequestUpdate
   *    - TransferRequestDelete
   */
  var messagesStack = List()
  
  def addCheckMessagesStack = {
    
  }
  
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

  var totalNewSiTransferred: Int = 0
  var totalScToSiTransferred: Int = 0

  def receive = {
    /**
     *  New HOSPITAL_STATE data to process
     */
    case HospitalState(elfin, transferActor) =>

      log.info(s"$name> HospitalActor(${name}) received new hospitalState schedule ${elfin.IDENTIFIANT.get.DE.get}")

      // Convert generic ELFIN data structure of CLASSE='HOSPITAL_STATE' to semantic type Hospital 
      val hospital = HospitalHelper.toHospital(elfin)

      //	    log.info(s"============================== $name - start ==============================")
      //	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
      //	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
      //	    log.info(s"------------------------------ $name --------------------------------------")

      // Roll hospital states
      previousHospitalState = currentHospitalState
      currentHospitalState = Some(hospital)

      /**
       *  - bedsWithIncomingPatientTypeSi should be transferred to PRT
       *  - bedsWithIncomingPatientTypeSc should stay at CDF
       *  - bedsWithOutgoingPatientTypeSi should not happen at CDF as SI patient are moved to PRT
       *  - bedsWithOutgoingPatientTypeSc are expected at CDF, we do nothing with it at the moment
       *  - patientTypeChangeFromScToSi should be transferred to PRT
       *  - patientTypeChangeFromSiToSc should never be present here as SI patients move to PRT
       *  - tranferTypeOnlyChange should replace their previous bed values with new updated ones
       */
      HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
        case (
          bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
          bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
          patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
          tranferTypeOnlyChange) =>

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
            totalNewSiTransferred = totalNewSiTransferred + bedsWithIncomingPatientTypeSi.size
            totalScToSiTransferred = totalScToSiTransferred + patientTypeChangeFromScToSi.size
          } else {
            // No transfer response to wait for, request next data.
            sender ! NextHospitalStatesRequest(name)
          }
      }

    //	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
    //	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
    //	    log.info(s"------------------------------ $name --------------------------------------")
    //	    log.info(s"$name> BedsWithIncomingPatient: " + HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState) )
    //	    log.info(s"$name> BedsWithOutgoingPatient: " + HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState) )
    //	    log.info(s"============================== $name - end   ==============================")

    case TransferResponse(id, status, acceptedIncomingBeds, fromHospital, toHospital, fromSchedule, message) => 
      status match {
        case TRANSFER_REQUEST_ACCEPTED =>
          log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_ACCEPTED, requesting next data.")
          log.info(s"TOTAL TRANSFERRED = ${totalNewSiTransferred + totalScToSiTransferred} , New SI = ${totalNewSiTransferred}, SC to SI = ${totalScToSiTransferred}")
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

    case DataSetEmpty => 
      sender ! WorkCompleted("HosptialActorCdf")
    
  }

}



