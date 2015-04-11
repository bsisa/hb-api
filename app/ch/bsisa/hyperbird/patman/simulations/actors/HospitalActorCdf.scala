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
class HospitalActorCdf(name: String, bedsNb: Int, simulatedHospitalStateReportActor: ActorRef) extends Actor with ActorLogging {

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
  var messageState: Option[CdfMessageState] = None

  /**
   * Check messages stack complete state
   */
  def checkMessageStateCompleted(msgStateOpt: Option[CdfMessageState]): Boolean = {

    msgStateOpt match {
      case Some(msgState) => if (msgState.tc.isDefined && msgState.tu.isDefined && msgState.td.isDefined) true else false
      case None => false
    }
  }

  /**
   * Trigger next HospitalState processing if available after having reset messageState to None
   */
  def requestNextDataAndResetMessageState() = {
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

  var totalNewSiTransferred: Int = 0
  var totalScToSiTransferred: Int = 0

  def receive = {
    /**
     *  New HOSPITAL_STATE data to process
     */
    case HospitalState(elfin, transferActor) =>

      log.info(s"$name> HospitalActor(${name}) received new hospitalState schedule ${elfin.IDENTIFIANT.get.DE.get}")

      //      // ========== Manage messageState ===============================
      //      // Init messageState
      //      if (!messageState.isDefined) {
      //        log.info(s"Initialising CdfMessageState")
      //      } else if ( checkMessageStateCompleted(messageState) ) {
      //        // Reset CdfMessageState with new HospitalState
      //        log.info(s"Reset CdfMessageState with new HospitalState")
      //        messageState = Some(CdfMessageState(HospitalState(elfin,transferActor), None, None, None))
      //      } else {
      //        val errMsg = s"$name> ERROR - HospitalActor(${name}) received new hospitalState schedule ${elfin.IDENTIFIANT.get.DE.get} before CdfMessageState completed!"
      //        log.error(errMsg)
      //        // Stop simulation while in unexpected state
      //        sender ! StopSimulationRequest(errMsg)
      //      }

      // Convert generic ELFIN data structure of CLASSE='HOSPITAL_STATE' to semantic type Hospital 
      val hospital = HospitalHelper.toHospital(elfin)

      // Roll hospital states
      previousHospitalState = currentHospitalState
      currentHospitalState = Some(hospital)

      // Manage messageState
      if (messageState.isEmpty) {
        messageState = Some(CdfMessageState(HospitalState(elfin, transferActor), tc = None, tu = None, td = None))
      } else {
        log.error(s"We do not expect SOME messageState while receiving a new HospitalState !!!")
      }

      /**
       * =====================================================================================================================================
       * ====                                      UPDATE ALGO CDF                                                                        ====
       * =====================================================================================================================================
       * These events are determined on static HOSPITAL_STATEs NOT on simulated hospital state.
       *
       *  - bedsWithIncomingPatientTypeSi must trigger TRANSFER NATURE="add"       => be transferred to PRT
       *  - patientTypeChangeFromScToSi   must trigger TRANSFER NATURE="add"       => be transferred to PRT
       *
       *  - bedsWithOutgoingPatientTypeSi must trigger TRANSFER NATURE="remove"    => notify PRT these transferred SI patients are going out
       *
       *  - patientTypeChangeFromSiToSc   must trigger TRANSFER NATURE="update"    => notify PRT patients have had their patient type changed
       *
       *  - tranferTypeOnlyChange:
       *       if SI patient type         must trigger TRANSFER NATURE="update"    => notify PRT patients have had transfer type changed
       *                                                                              (replace their previous bed values with new updated ones)
       *       if SC patient type         must update CDF `simulatedHospitalState` => replace their previous bed values with new updated ones
       *
       *  - bedsWithIncomingPatientTypeSc must update CDF `simulatedHospitalState` => stay at CDF
       *  - bedsWithOutgoingPatientTypeSc must update CDF `simulatedHospitalState` => out of CDF
       *
       *
       *
       */
      HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
        case (
          bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
          bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
          patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
          bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

          // ================================================================================================================================= 
          // Update current CDT simulatedHospitalState removing transfered SI beds
          // =================================================================================================================================
          // TODO: Full implementation review needed. Follow above algorithm description (UPDATE ALGO CDF)
          simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForCdf(
            currentSimulatedHospitalStateOption = simulatedHospitalState,
            newStaticHospitalStateOption = currentHospitalState,
            bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
            bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
            patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
            bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc)

          log.info(s"${name}> SIMULATED HS: ${simulatedHospitalState}")
          // Send message to create simulatedHospitalState entry for the current state.
          simulatedHospitalStateReportActor ! SimulatedHospitalState(hospitalState = simulatedHospitalState.get)

          // =================================================================================================================================
          // Always send SI movements as TransferRequestCreate, TransferRequestUpdate, TransferRequestDelete to PRT
          // =================================================================================================================================          
          // This is necessary even if not beds are involved to complete the expected messageState checkMessageStateCompleted.
          // Note: Possible minor optimisation: we could directly update the messageState and bypass unnecessary message.

          // 1) TransferRequestCreate  =======================================================================================================
          // - bedsWithIncomingPatientTypeSi must trigger TRANSFER NATURE="add"       => be transferred to PRT
          // - patientTypeChangeFromScToSi   must trigger TRANSFER NATURE="add"       => be transferred to PRT            

          // Compute list of beds to transfer (+)
          //val bedsToAddTransfer = bedsWithIncomingPatientTypeSi ++ patientTypeChangeFromScToSi
          val tranferReqCreate = TransferRequestCreate(
            id = elfin.Id, bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi = patientTypeChangeFromScToSi,
            fromHospitalCode = HOSPITAL_CODE_CDF, toHospitalCode = HOSPITAL_CODE_PRT, fromSchedule = hospital.schedule,
            message = s"Requesting SI TransferRequestCreate for ${hospital.schedule} from ${HOSPITAL_CODE_CDF} to ${HOSPITAL_CODE_PRT} with: ${bedsWithIncomingPatientTypeSi.size} incoming SI and ${patientTypeChangeFromScToSi.size} type change SC to SI.")

          transferActor ! tranferReqCreate

          // 2) TransferRequestUpdate  =======================================================================================================
          // patientTypeChangeFromSiToSc   must trigger TRANSFER NATURE="update"    => notify PRT patients have had their patient type changed
          // tranferTypeOnlyChange:
          //    if SI patient type         must trigger TRANSFER NATURE="update"    => notify PRT patients have had transfer type changed
          //                                                                          (replace their previous bed values with new updated ones)            
          //val bedsToUpdateTransfer = patientTypeChangeFromSiToSc ++ bedsWithTransferTypeOnlyChangePatientTypeSi              
          val transferReqUpdate = TransferRequestUpdate(
            id = elfin.Id, patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi,
            fromHospitalCode = HOSPITAL_CODE_CDF, toHospitalCode = HOSPITAL_CODE_PRT, fromSchedule = hospital.schedule,
            message = s"Requesting SI TransferRequestUpdate for ${hospital.schedule} from ${HOSPITAL_CODE_CDF} to ${HOSPITAL_CODE_PRT} with: ${patientTypeChangeFromSiToSc.size} SI to SC and ${bedsWithTransferTypeOnlyChangePatientTypeSi.size} transfer type change.")

          transferActor ! transferReqUpdate

          // 3) TransferRequestDelete  =======================================================================================================
          // bedsWithOutgoingPatientTypeSi must trigger TRANSFER NATURE="remove"    => notify PRT these transferred SI patients are going out
          // bedsWithOutgoingPatientTypeSc must also be included in case they were previously SI transferred to PRT with SI to SC type change. 
          //val bedsToDeleteTransfer = bedsWithOutgoingPatientTypeSi
          val transferReqDelete = TransferRequestDelete(
            id = elfin.Id, bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc,
            fromHospitalCode = HOSPITAL_CODE_CDF, toHospitalCode = HOSPITAL_CODE_PRT, fromSchedule = hospital.schedule,
            message = s"Requesting SI TransferRequestDelete for ${hospital.schedule} from ${HOSPITAL_CODE_CDF} to ${HOSPITAL_CODE_PRT} with: ${bedsWithOutgoingPatientTypeSi.size} outgoing SI and 0 to ${bedsWithOutgoingPatientTypeSc.size} outgoing SC (if formerly SI).")

          transferActor ! transferReqDelete

          // Send SI movements as Transfer requests to PRT only if necessary
//          if (bedsWithIncomingPatientTypeSi != Nil || bedsWithOutgoingPatientTypeSi != Nil || patientTypeChangeFromScToSi != Nil) {
//            transferActor ! TransferRequest(
//              id = elfin.Id,
//              incomingSiBeds = bedsWithIncomingPatientTypeSi,
//              outgoingSiBeds = bedsWithOutgoingPatientTypeSi,
//              typeScToSiBeds = patientTypeChangeFromScToSi,
//              fromHospitalCode = HOSPITAL_CODE_CDF,
//              toHospitalCode = HOSPITAL_CODE_PRT,
//              fromSchedule = hospital.schedule,
//              message = s"Requesting SI transfer for ${hospital.schedule} from ${HOSPITAL_CODE_CDF} to ${HOSPITAL_CODE_PRT} with:\n+ ${bedsWithIncomingPatientTypeSi.size} in, - ${bedsWithOutgoingPatientTypeSi.size} out, + ${patientTypeChangeFromScToSi.size} SC to SI in")
//            totalNewSiTransferred = totalNewSiTransferred + bedsWithIncomingPatientTypeSi.size
//            totalScToSiTransferred = totalScToSiTransferred + patientTypeChangeFromScToSi.size
//          } else {
//            // No transfer response to wait for, request next data.
//            sender ! NextHospitalStatesRequest(name)
//          }
      }

    //      sender ! HospitalStateOk(elfin = elfin, fromHospital = name, previousSimulatedHospitalState = simulatedHospitalState)

    //    case ComputeSimulatedState(elfin, transferActor, previousPrtSimulatedHospitalState) =>
    //      
    //            // Convert generic ELFIN data structure of CLASSE='HOSPITAL_STATE' to semantic type Hospital 
    //      val hospital = HospitalHelper.toHospital(elfin)
    //      
    ////      
    ////      // TODO: Refactor HospitalHelper.getBedsUpdates logic
    ////      // 
    ////      // Incoming patients are those who do not already exist at CDF nor PRT, either as SI or SC
    ////      // => create TransferRequestCreate if of type SI at CDF
    ////      // or those who do already exist at CDF as SC and changed to SI. They  must be moved to PRT and 
    ////      // removed from CDF.
    ////      // 
    ////      // Updated patient are those who have been: 
    ////      // A) Transferred to PRT and are either updated and still SI or updated and possibly changed to SC
    ////      // B) At CDF as SC and updated
    ////      // => create TransferRequestUpdate if of type SI or SC at PRT (follow up of CDF SI patients
    ////      //    transferred to PRT)
    ////      //
    ////      // Outgoing patients identification at CDF must check not found at previous HS at CDF nor  
    ////      // 
    ////      
    //      /**
    //       *  - bedsWithIncomingPatientTypeSi should be transferred to PRT
    //       *  - bedsWithIncomingPatientTypeSc should stay at CDF
    //       *  - bedsWithOutgoingPatientTypeSi should not happen at CDF as SI patient are moved to PRT
    //       *  - bedsWithOutgoingPatientTypeSc are expected at CDF, we do nothing with it at the moment
    //       *  - patientTypeChangeFromScToSi should be transferred to PRT
    //       *  - patientTypeChangeFromSiToSc should never be present here as SI patients move to PRT
    //       *  - tranferTypeOnlyChange should replace their previous bed values with new updated ones
    //       */
    //      HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    //        case (
    //          bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
    //          bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
    //          patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
    //          tranferTypeOnlyChange) =>
    //
    //            // 1) TransferRequestCreate
    //            // 2) TransferRequestUpdate
    //            // 3) TransferRequestDelete
    //            
    //          // Update current CDT simulatedHospitalState removing transfered SI beds
    //          simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForCdf(
    //            currentSimulatedHospitalStateOption = simulatedHospitalState,
    //            newStaticHospitalStateOption = currentHospitalState,
    //            bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
    //            bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, patientTypeChangeFromScToSi,
    //            patientTypeChangeFromSiToSc, tranferTypeOnlyChange)
    //
    //          log.info(s"${name}> SIMULATED HS: ${simulatedHospitalState}")
    //          // TODO: send message to create simulatedHospitalState entry for the current state.
    //
    //          // Send SI movements as Transfer requests to PRT only if necessary
    //          if (bedsWithIncomingPatientTypeSi != Nil || bedsWithOutgoingPatientTypeSi != Nil || patientTypeChangeFromScToSi != Nil) {
    //            transferActor ! TransferRequest(
    //              id = elfin.Id,
    //              incomingSiBeds = bedsWithIncomingPatientTypeSi,
    //              outgoingSiBeds = bedsWithOutgoingPatientTypeSi,
    //              typeScToSiBeds = patientTypeChangeFromScToSi,
    //              fromHospitalCode = HOSPITAL_CODE_CDF,
    //              toHospitalCode = HOSPITAL_CODE_PRT,
    //              fromSchedule = hospital.schedule,
    //              message = s"Requesting SI transfer for ${hospital.schedule} from ${HOSPITAL_CODE_CDF} to ${HOSPITAL_CODE_PRT} with:\n+ ${bedsWithIncomingPatientTypeSi.size} in, - ${bedsWithOutgoingPatientTypeSi.size} out, + ${patientTypeChangeFromScToSi.size} SC to SI in")
    //            totalNewSiTransferred = totalNewSiTransferred + bedsWithIncomingPatientTypeSi.size
    //            totalScToSiTransferred = totalScToSiTransferred + patientTypeChangeFromScToSi.size
    //          } else {
    //            // No transfer response to wait for, request next data.
    //            sender ! NextHospitalStatesRequest(name)
    //          }
    //      }

    case  TransferResponseCreate(correlationId, status, fromHospitalCode, toHospitalCode, message) =>
      log.info(s"Received TransferResponseCreate($correlationId, $message)")
      messageState = messageState match {
        case Some(CdfMessageState(hs, tc, tu, td)) => Some(CdfMessageState(hs, Some(TransferResponseCreate(correlationId, status, fromHospitalCode, toHospitalCode, message)), tu, td))
        case None => None
      }
      if (checkMessageStateCompleted(messageState)) requestNextDataAndResetMessageState()

    case TransferResponseUpdate(correlationId, status, fromHospitalCode, toHospitalCode, message) =>
      log.info(s"Received TransferResponseUpdate($correlationId, $message)")
      messageState = messageState match {
        case Some(CdfMessageState(hs, tc, tu, td)) => Some(CdfMessageState(hs, tc, Some(TransferResponseUpdate(correlationId, status, fromHospitalCode, toHospitalCode, message)), td))
        case None => None
      }
      if (checkMessageStateCompleted(messageState)) requestNextDataAndResetMessageState()

    case TransferResponseDelete(correlationId, status, fromHospitalCode, toHospitalCode, message) =>
      log.info(s"Received TransferResponseDelete($correlationId, $message)")
      messageState = messageState match {
        case Some(CdfMessageState(hs, tc, tu, td)) => Some(CdfMessageState(hs, tc, tu, Some(TransferResponseDelete(correlationId, status, fromHospitalCode, toHospitalCode, message))))
        case None => None
      }
      if (checkMessageStateCompleted(messageState)) requestNextDataAndResetMessageState()

//    case TransferResponse(id, status, acceptedIncomingBeds, fromHospital, toHospital, fromSchedule, message) =>
//      status match {
//        case TRANSFER_REQUEST_ACCEPTED =>
//          log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_ACCEPTED, requesting next data.")
//          log.info(s"TOTAL TRANSFERRED = ${totalNewSiTransferred + totalScToSiTransferred} , New SI = ${totalNewSiTransferred}, SC to SI = ${totalScToSiTransferred}")
//          // Request next data.
//          context.parent ! NextHospitalStatesRequest(name)
//        case TRANSFER_REQUEST_REFUSED =>
//          // We should not obtain this
//          log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_REFUSED")
//          context.parent ! StopSimulationRequest(s"TransferRequest id = $id : TRANSFER_REQUEST_REFUSED")
//        case TRANSFER_REQUEST_PARTIAL =>
//          // We should not obtain this
//          log.info(s"TransferRequest id = $id : TRANSFER_REQUEST_PARTIAL")
//          context.parent ! StopSimulationRequest(s"TransferRequest id = $id : TRANSFER_REQUEST_PARTIAL")
//      }

    case DataSetEmpty =>
      sender ! WorkCompleted("HosptialActorCdf")

  }

}

/**
 * Contains latest states to manage data loop
 */
case class CdfMessageState(hs: HospitalState, tc: Option[TransferResponseCreate], tu: Option[TransferResponseUpdate], td: Option[TransferResponseDelete]) {

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


