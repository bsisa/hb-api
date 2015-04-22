package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.patman.simulations.model.HospitalSimulationSummary

/**
 * Models PRT Hospital intensive care data as bed, patient, patient type, transfer type
 * and related behaviour intended for simulation.
 */
class HospitalActorPrt(name: String, bedsNb: Int, simulatedHospitalStateReportActor: ActorRef) extends Actor with ActorLogging {

  
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
  var messageState: Option[PrtMessageState] = None  
  
  /**
   * Check messages stack complete state
   */
  def checkMessageStateCompleted(msgStateOpt: Option[PrtMessageState]): Boolean = {

    msgStateOpt match {
      case Some(msgState) => if (msgState.tc.isDefined && msgState.tu.isDefined && msgState.td.isDefined) true else false
      case None => false
    }
  }  
  
  /**
   * Trigger next HospitalState processing if available after having reset messageState to None
   * and sent 
   */
  def requestNextDataAndSendStateToReportAndResetMessageState() = {
    
    // Now that all updates have been applied to simulatedHospitalState send current simulated state to report actor
    log.info(s"${name}> SIMULATED HS: ${simulatedHospitalState}")
    simulatedHospitalStateReportActor ! SimulatedHospitalState(hospitalState = simulatedHospitalState.get)    
    
    messageState = None
    // Request next data.
    context.parent ! NextHospitalStatesRequest(name)
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

  /**
   * Maintained hospital aggregated figures delivered at simulation end.
   */
  var simulationSummary : Option[HospitalSimulationSummary] = None
  
  /**
   * A data loop includes:
   * - HospitalState (1) 		=> new state
   * - TransferRequest (0-n)	=> new SI patients from CDF
   * - UpdateState (0-n)		=> purely technical: Keeps transferred beds status updated with event recorded at CDF.
   *                           	May notify outgoing patient (bed to remove), patient type change (SI => SC),
   *                           	transfer type change.
   *
   */
  def receive = {
    case HospitalState(elfin, transferActor) => {
      log.info(s"$name> HospitalActor(${name}) received new hospitalState schedule ${elfin.IDENTIFIANT.get.DE.get}")
      val hospital = HospitalHelper.toHospital(elfin)
      //	    log.info(s"============================== $name - start ==============================")
      //	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
      //	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
      //	    log.info(s"------------------------------ $name --------------------------------------")

      // Roll states
      previousHospitalState = currentHospitalState
      currentHospitalState = Some(hospital)

      //      sender ! HospitalStateOk(elfin = elfin, fromHospital = name, previousSimulatedHospitalState = simulatedHospitalState) 

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
          bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) => {

          // Update hospital simulation summary
          simulationSummary = Some(
              HospitalHelper.updateHospitalSimulationSummary(
                  hospitalCode = name,
                  currentHss = simulationSummary, 
                  bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, 
                  bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, 
                  bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, 
                  bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc))            
            
          // Update current PRT simulatedHospitalState tracking `static` PRT changes only
          simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForPrt(
            currentSimulatedHospitalStateOption = simulatedHospitalState,
            newStaticHospitalStateOption = currentHospitalState,
            bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
            bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, 
            patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc, 
            bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc)

          // TODO: this must not be done before both `static` and `dynamic` PRT changes have been applied to simulatedHospitalState
          // TODO: fix: log.info(s"${name}> SIMULATED HS: ${simulatedHospitalState}")
          // TODO: fix: simulatedHospitalStateReportActor ! SimulatedHospitalState(hospitalState = simulatedHospitalState.get)
        }

      }

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
    }

    //    case ComputeSimulatedState(elfin, transferActor, previousCdfSimulatedHospitalState) => {
    //                  // Convert generic ELFIN data structure of CLASSE='HOSPITAL_STATE' to semantic type Hospital 
    //      val hospital = HospitalHelper.toHospital(elfin)
    //      log.info(s"ComputeSimulatedState - NOT IMPLEMENTED YET...")
    //    }

    case TransferRequestCreate(id, bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      
      // TODO: UPDATE SIMULATED STATE
      
      log.info(message)
      val tRespCreate = TransferResponseCreate(correlationId = id, status = true, fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, message = s"TransferResponseCreate for id ${id} accepted.")
      sender ! tRespCreate
      // Update message state
      messageState = messageState match {
        case Some(PrtMessageState(hs, tc, tu, td)) => Some(PrtMessageState(hs, Some(tRespCreate), tu, td))
        case None => None
      }      
      // Check message state
      if (checkMessageStateCompleted(messageState)) requestNextDataAndSendStateToReportAndResetMessageState()      
      
      
    case TransferRequestUpdate(id, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      
      // TODO: UPDATE SIMULATED STATE
      
      log.info(message)
      
      val tRespUpdate = TransferResponseUpdate(correlationId = id, status = true, fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, message = s"TransferRequestUpdate for id ${id} accepted.") 
      sender ! tRespUpdate
      // Update message state
      messageState = messageState match {
        case Some(PrtMessageState(hs, tc, tu, td)) => Some(PrtMessageState(hs, tc, Some(tRespUpdate), td))
        case None => None
      }      
      // Check message state
      if (checkMessageStateCompleted(messageState)) requestNextDataAndSendStateToReportAndResetMessageState()
      
    case TransferRequestDelete(id, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      
      // TODO: UPDATE SIMULATED STATE
      
      log.info(message)
      
      val tRespDelete = TransferResponseDelete(correlationId = id, status = true, fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, message = s"TransferRequestDelete for id ${id} accepted.") 
      sender ! tRespDelete
      // Update message state
      messageState = messageState match {
        case Some(PrtMessageState(hs, tc, tu, td)) => Some(PrtMessageState(hs, tc, tu, Some(tRespDelete)))
        case None => None
      }      
      // Check message state
      if (checkMessageStateCompleted(messageState)) requestNextDataAndSendStateToReportAndResetMessageState()
      
    
//    case TransferRequest(id, incomingSiBeds, outgoingSiBeds, typeScToSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
//      log.info(message)
//      //log.info(s"Request for transfer from ${fromHospitalCode} to ${toHospitalCode} with in = ${incomingSiBeds.size}, out = ${outgoingSiBeds.size}, typeScToSiBeds = ${typeScToSiBeds.size} DELTA = ${incomingSiBeds.size - outgoingSiBeds.size + typeScToSiBeds.size}")
//      // OBSOLETE TODO: Request DataSet update for new Si beds => incomingSiBeds + typeScToSiBeds
//      val allTransferredSiBeds = incomingSiBeds ::: typeScToSiBeds
//      //sender ! DataSetUpdateRequest(id, allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule)
//
//      // =========================================================================================================
//      // TODO: Notify TransferReportActor of transfer events: Hospital from => to, Schedule, PatientNb, PatientType, TransferType, Reason for transfer: New SI or SC to SI
//
//      // TODO: Update current PRT hospital state with new SI beds (outgoingSiBeds should be made OBSOLETE and always be 0 (Once DataSet update is implemented))
//
//      // =========================================================================================================          
//      // TODO: Confirm TRANSFER_REQUEST_ACCEPTED with TransferResponse to TransferActor
//      sender ! TransferResponse(id, TRANSFER_REQUEST_ACCEPTED, allTransferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule, "Transfer report notification (TODO) and HospitalActor PRT update (TODO) succeeded.")
//      // Request next data.
//      context.parent ! NextHospitalStatesRequest(name)
//
//    case TransferResponse(id, status, acceptedIncomingBeds, fromHospital, toHospital, fromSchedule, message) =>
//      // No transfer request from PRT at the moment
//      log.warning(s"Unexpected transferResponse at PRT: id = $id, $message")

    case DataSetEmpty =>
      // TODO: provide agreggates to store in SIMULATION
      sender ! WorkCompleted("HosptialActorPrt", simulationSummary)

  }

}

/**
 * Contains latest states to manage data loop
 */
case class PrtMessageState(hs: HospitalState, tc: Option[TransferResponseCreate], tu: Option[TransferResponseUpdate], td: Option[TransferResponseDelete]) {

}

