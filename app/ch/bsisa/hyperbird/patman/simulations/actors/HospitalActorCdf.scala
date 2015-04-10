package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
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
  var messageState : Option[CdfMessageState] = None
  
  /**
   * Check messages stack complete state
   */
  def checkMessageStateCompleted( msgStateOpt: Option[CdfMessageState]) : Boolean = {

    msgStateOpt match {
      case Some(msgState) => if (msgState.tc.isDefined && msgState.tu.isDefined && msgState.td.isDefined) true else false
      case None => false
    }
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
      
      // ========== Manage messageState ===============================
      // Init messageState
      if (!messageState.isDefined) {
        log.info(s"Initialising CdfMessageState")
      } else if ( checkMessageStateCompleted(messageState) ) {
        // Reset CdfMessageState with new HospitalState
        log.info(s"Reset CdfMessageState with new HospitalState")
        messageState = Some(CdfMessageState(HospitalState(elfin,transferActor), None, None, None))
      } else {
        val errMsg = s"$name> ERROR - HospitalActor(${name}) received new hospitalState schedule ${elfin.IDENTIFIANT.get.DE.get} before CdfMessageState completed!"
        log.error(errMsg)
        // Stop simulation while in unexpected state
        sender ! StopSimulationRequest(errMsg)
      }

      // Convert generic ELFIN data structure of CLASSE='HOSPITAL_STATE' to semantic type Hospital 
      val hospital = HospitalHelper.toHospital(elfin)



      // Roll hospital states
      previousHospitalState = currentHospitalState
      currentHospitalState = Some(hospital)

      sender ! HospitalStateOk(elfin = elfin, fromHospital = name, previousSimulatedHospitalState = simulatedHospitalState)


    case ComputeSimulatedState(elfin, transferActor, previousPrtSimulatedHospitalState) =>
      
            // Convert generic ELFIN data structure of CLASSE='HOSPITAL_STATE' to semantic type Hospital 
      val hospital = HospitalHelper.toHospital(elfin)
      
//      
//      // TODO: Refactor HospitalHelper.getBedsUpdates logic
//      // 
//      // Incoming patients are those who do not already exist at CDF nor PRT, either as SI or SC
//      // => create TransferRequestCreate if of type SI at CDF
//      // or those who do already exist at CDF as SC and changed to SI. They  must be moved to PRT and 
//      // removed from CDF.
//      // 
//      // Updated patient are those who have been: 
//      // A) Transferred to PRT and are either updated and still SI or updated and possibly changed to SC
//      // B) At CDF as SC and updated
//      // => create TransferRequestUpdate if of type SI or SC at PRT (follow up of CDF SI patients
//      //    transferred to PRT)
//      //
//      // Outgoing patients identification at CDF must check not found at previous HS at CDF nor  
//      // 
//      
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

            // 1) TransferRequestCreate
            // 2) TransferRequestUpdate
            // 3) TransferRequestDelete
            
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
      
      
      
      
    case TransferResponseCreate(correlationId, status, message) => 
      log.info(s"Received TransferResponseCreate($correlationId, $message)")
    case TransferResponseUpdate(correlationId, status, message) => 
      log.info(s"Received TransferResponseUpdate($correlationId, $message)")
    case TransferResponseDelete(correlationId, status, message) => 
      log.info(s"Received TransferResponseDelete($correlationId, $message)")
      
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

/**
 * Contains latest states to manage data loop
 */
case class CdfMessageState(hs:HospitalState, tc:Option[TransferRequestCreate] ,tu:Option[TransferRequestUpdate], td:Option[TransferRequestDelete]) {
  
}


//	    log.info(s"============================== $name - start ==============================")
//	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
//	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
//	    log.info(s"------------------------------ $name --------------------------------------")
//	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
//	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
//	    log.info(s"------------------------------ $name --------------------------------------")
//	    log.info(s"$name> BedsWithIncomingPatient: " + HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState) )
//	    log.info(s"$name> BedsWithOutgoingPatient: " + HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState) )
//	    log.info(s"============================== $name - end   ==============================")


