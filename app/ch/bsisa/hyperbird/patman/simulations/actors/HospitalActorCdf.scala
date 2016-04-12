package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{ Actor, ActorRef, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.patman.simulations.model.HospitalSimulationSummary

/**
 * Models CDF Hospital intensive care data such as bed, patient, patient type, transfer type
 * and related behaviour intended for simulation.
 */
class HospitalActorCdf(name: String, bedsNb: Int, simulatedHospitalStateReportActor: ActorRef) extends Actor with ActorLogging {

  /**
   * Messages stack life cycle
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

  /**
   * Maintained hospital aggregated figures delivered at simulation end.
   */
  var simulationSummary: Option[HospitalSimulationSummary] = None


  /**
   * Actor received message trait implementation
   */
  def receive = {
    /**
     *  New HOSPITAL_STATE data to process
     */
    case HospitalState(elfin, transferActor) =>

      log.info(s"$name> HospitalActor(${name}) received new hospitalState schedule ${elfin.IDENTIFIANT.get.DE.get}")

      // Convert generic ELFIN data structure of CLASSE='HOSPITAL_STATE' to semantic type Hospital 
      val hospital = HospitalHelper.toHospital(elfin)

      // Roll hospital states
      previousHospitalState = currentHospitalState
      currentHospitalState = Some(hospital)

      // Manage messageState
      if (messageState.isEmpty) {
        messageState = Some(CdfMessageState(HospitalState(elfin, transferActor), tc = None, tu = None, td = None))
      } else {
    	  log.error(s">CDF: We do not expect SOME messageState while receiving a new HospitalState !!!")
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
       *       if SC patient type         
       *              if was formerly SI patient type (see #4 Patient management simulation creates duplicate entries in specific conditions with minor impact)
       *                                  must trigger TRANSFER NATURE="update"    => notify PRT patients have had transfer type changed
       *                                                                              (replace their previous bed values with new updated ones)
       *              else
       *                                  must update CDF `simulatedHospitalState` => replace their previous bed values with new updated ones 
       *
       *  - bedsWithIncomingPatientTypeSc must update CDF `simulatedHospitalState` => stay at CDF
       *  - bedsWithOutgoingPatientTypeSc must update CDF `simulatedHospitalState` => out of CDF
       *
       */
      HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
        case (
          bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
          bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
          patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
          bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

          // Fix #4 by adding missing 'remoteBedsWithTransferTypeOnlyChangePatientTypeSc' and  'localBedsWithTransferTypeOnlyChangePatientTypeSc'
          // Indeed formerly SI at CDF that have changed to SC need remote update request when their transfer type changes 
          // similarly to 'bedsWithTransferTypeOnlyChangePatientTypeSi' while local beds can keep the same processing logic. 
          val remoteBedsWithTransferTypeOnlyChangePatientTypeSc = simulatedHospitalState match {
            // Keep only beds which are not managed in local simulated hospital state. These are managed at remote end (PRT site).
            case Some(localSimulatedHospitalState) => bedsWithTransferTypeOnlyChangePatientTypeSc diff localSimulatedHospitalState.beds
            case None                              => bedsWithTransferTypeOnlyChangePatientTypeSc // This should be empty list
          }
            // Keep only beds which are managed in local simulated hospital state. These are managed at local end (CDF site).
          val localBedsWithTransferTypeOnlyChangePatientTypeSc = simulatedHospitalState match {
            case Some(localSimulatedHospitalState) => bedsWithTransferTypeOnlyChangePatientTypeSc intersect localSimulatedHospitalState.beds
            case None                              => bedsWithTransferTypeOnlyChangePatientTypeSc // This should be empty list
          }

          // Update hospital simulation summary
          simulationSummary = Some(
              HospitalHelper.updateHospitalSimulationSummary(
                  hospitalCode = name,
                  currentHss = simulationSummary, 
                  bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, 
                  bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, 
                  bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, 
                  bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc))

          // ================================================================================================================================= 
          // Update current CDT simulatedHospitalState removing transferred SI beds
          // =================================================================================================================================
          simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForCdf(
            currentSimulatedHospitalStateOption = simulatedHospitalState,
            newStaticHospitalStateOption = currentHospitalState,
            bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
            bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
            patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
            bedsWithTransferTypeOnlyChangePatientTypeSi, localBedsWithTransferTypeOnlyChangePatientTypeSc)

          log.info(s"${name}> SIMULATED HS: ${simulatedHospitalState}")
          // Send message to create simulatedHospitalState entry for the current state.
          simulatedHospitalStateReportActor ! SimulatedHospitalState(hospitalState = simulatedHospitalState.get)

          // =================================================================================================================================
          // Always send SI movements as TransferRequestCreate, TransferRequestUpdate, TransferRequestDelete to PRT
          // =================================================================================================================================          
          // This is necessary even if no beds are involved to complete the expected messageState checkMessageStateCompleted.
          // Note: Possible minor optimisation: we could directly update the messageState and bypass unnecessary message.

          // 1) TransferRequestCreate  =======================================================================================================
          // - bedsWithIncomingPatientTypeSi must trigger TRANSFER NATURE="add"       => be transferred to PRT
          // - patientTypeChangeFromScToSi   must trigger TRANSFER NATURE="add"       => be transferred to PRT            
          val tranferReqCreate = TransferRequestCreate(
            id = elfin.Id, bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi = patientTypeChangeFromScToSi,
            fromHospitalCode = HOSPITAL_CODE_CDF, toHospitalCode = HOSPITAL_CODE_PRT, fromSchedule = hospital.schedule,
            message = s"Requesting SI TransferRequestCreate for ${hospital.schedule} from ${HOSPITAL_CODE_CDF} to ${HOSPITAL_CODE_PRT} with: ${bedsWithIncomingPatientTypeSi.size} incoming SI and ${patientTypeChangeFromScToSi.size} type change SC to SI.")

          transferActor ! tranferReqCreate

          // 2) TransferRequestUpdate  =======================================================================================================
          // patientTypeChangeFromSiToSc   must trigger TRANSFER NATURE="update"    => notify PRT patients have had their patient type changed

          //  - tranferTypeOnlyChange:
          //       if SI patient type         must trigger TRANSFER NATURE="update"    => notify PRT patients have had transfer type changed
          //                                                                              (replace their previous bed values with new updated ones)
          //       if SC patient type         
          //              if was formerly SI patient type (see #4 Patient management simulation creates duplicate entries in specific conditions with minor impact)
          //                                  must trigger TRANSFER NATURE="update"    => notify PRT patients have had transfer type changed
          //                                                                              (replace their previous bed values with new updated ones)
          //              else
          //                                  must update CDF `simulatedHospitalState` => replace their previous bed values with new updated ones
          val transferReqUpdate = TransferRequestUpdate(
            id = elfin.Id, patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi,
            bedsWithTransferTypeOnlyChangePatientTypeSc = remoteBedsWithTransferTypeOnlyChangePatientTypeSc,
            fromHospitalCode = HOSPITAL_CODE_CDF, toHospitalCode = HOSPITAL_CODE_PRT, fromSchedule = hospital.schedule,
            message = s"Requesting SI TransferRequestUpdate for ${hospital.schedule} from ${HOSPITAL_CODE_CDF} to ${HOSPITAL_CODE_PRT} with: ${patientTypeChangeFromSiToSc.size} SI to SC and ${bedsWithTransferTypeOnlyChangePatientTypeSi.size} transfer type change.")

          transferActor ! transferReqUpdate

          // 3) TransferRequestDelete  =======================================================================================================
          // bedsWithOutgoingPatientTypeSi must trigger TRANSFER NATURE="remove"    => notify PRT these transferred SI patients are going out
          // bedsWithOutgoingPatientTypeSc must also be included in case they were previously SI transferred to PRT with SI to SC type change. 
          val transferReqDelete = TransferRequestDelete(
            id = elfin.Id, bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc,
            fromHospitalCode = HOSPITAL_CODE_CDF, toHospitalCode = HOSPITAL_CODE_PRT, fromSchedule = hospital.schedule,
            message = s"Requesting SI TransferRequestDelete for ${hospital.schedule} from ${HOSPITAL_CODE_CDF} to ${HOSPITAL_CODE_PRT} with: ${bedsWithOutgoingPatientTypeSi.size} outgoing SI and 0 to ${bedsWithOutgoingPatientTypeSc.size} outgoing SC (if formerly SI).")

          transferActor ! transferReqDelete

      }

    case TransferResponseCreate(correlationId, status, fromHospitalCode, toHospitalCode, message) =>
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

    case DataSetEmpty =>
      sender ! WorkCompleted("HosptialActorCdf", simulationSummary)

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


