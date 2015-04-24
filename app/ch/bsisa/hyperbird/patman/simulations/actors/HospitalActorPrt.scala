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
      case Some(msgState) =>
        if (msgState.tc.isDefined && msgState.tu.isDefined && msgState.td.isDefined) true else false
      case None =>
        false
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
    // Reset message state
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

      // Convert HOSPITAL_STATE ELFIN in XML format to semantic Hospital type
      val hospital = HospitalHelper.toHospital(elfin)

      // Roll states
      previousHospitalState = currentHospitalState
      currentHospitalState = Some(hospital)

      // Manage messageState
      messageState = messageState match {
        case Some(PrtMessageState(hs, tc, tu, td)) => Some(PrtMessageState(Some(HospitalState(elfin, transferActor)), tc, tu, td))
        case None => Some(PrtMessageState(Some(HospitalState(elfin, transferActor)), tc = None, tu = None, td = None))
      }

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

        }
      }
      // Check message state
      if (checkMessageStateCompleted(messageState)) requestNextDataAndSendStateToReportAndResetMessageState()
    }

    
    case TransferRequestCreate(id, bedsWithIncomingPatientTypeSi, patientTypeChangeFromScToSi, fromHospitalCode, toHospitalCode, fromSchedule, message) =>

      log.debug(message)
      
      // Update hospital simulation summary
      // Sum up CDF incoming SI and CDF patient type change from SC to SI which are all transferred here at PRT
      val incomingTransferredSi = bedsWithIncomingPatientTypeSi ++ patientTypeChangeFromScToSi
      simulationSummary = Some(
        HospitalHelper.updateHospitalSimulationSummary(
          hospitalCode = name,
          currentHss = simulationSummary,
          bedsWithIncomingPatientTypeSi = incomingTransferredSi,
          bedsWithIncomingPatientTypeSc = List(),
          bedsWithOutgoingPatientTypeSi = List(),
          bedsWithOutgoingPatientTypeSc = List()))

      // Update current PRT simulatedHospitalState tracking `dynamic` incoming transferred patients
      simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForPrt(
        currentSimulatedHospitalStateOption = simulatedHospitalState,
        newStaticHospitalStateOption = currentHospitalState,
        bedsWithIncomingPatientTypeSi = incomingTransferredSi,
        bedsWithIncomingPatientTypeSc = List(),
        bedsWithOutgoingPatientTypeSi = List(),
        bedsWithOutgoingPatientTypeSc = List(),
        patientTypeChangeFromScToSi = List(),
        patientTypeChangeFromSiToSc = List(),
        bedsWithTransferTypeOnlyChangePatientTypeSi = List(),
        bedsWithTransferTypeOnlyChangePatientTypeSc = List())

      val tRespCreate = TransferResponseCreate(correlationId = id, status = true, fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, message = s"TransferResponseCreate for id ${id} accepted.")
      sender ! tRespCreate
      
      // Manage messageState
      messageState = messageState match {
        case Some(PrtMessageState(hs, tc, tu, td)) => Some(PrtMessageState(hs, Some(tRespCreate), tu, td))
        case None => Some(PrtMessageState(hs = None, tc = Some(tRespCreate), tu = None, td = None))
      }
      
      // Check message state
      if (checkMessageStateCompleted(messageState)) requestNextDataAndSendStateToReportAndResetMessageState()

      
    case TransferRequestUpdate(id, patientTypeChangeFromSiToSc, bedsWithTransferTypeOnlyChangePatientTypeSi, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      log.debug(message)

      // Remark: TransferRequestUpdate track tranferred SI changes and does not lead to any hospital simulation summary update

      // Update current PRT simulatedHospitalState tracking `dynamic` updates of transferred patients
      simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForPrt(
        currentSimulatedHospitalStateOption = simulatedHospitalState,
        newStaticHospitalStateOption = currentHospitalState,
        bedsWithIncomingPatientTypeSi = List(),
        bedsWithIncomingPatientTypeSc = List(),
        bedsWithOutgoingPatientTypeSi = List(),
        bedsWithOutgoingPatientTypeSc = List(),
        patientTypeChangeFromScToSi = List(),
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi,
        bedsWithTransferTypeOnlyChangePatientTypeSc = List())

      val tRespUpdate = TransferResponseUpdate(correlationId = id, status = true, fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, message = s"TransferRequestUpdate for id ${id} accepted.")
      sender ! tRespUpdate
      
      // Manage messageState
      messageState = messageState match {
        case Some(PrtMessageState(hs, tc, tu, td)) => Some(PrtMessageState(hs, tc, Some(tRespUpdate), td))
        case None => Some(PrtMessageState(hs = None, tc = None, tu = Some(tRespUpdate), td = None))
      }      
      
      // Check message state
      if (checkMessageStateCompleted(messageState)) requestNextDataAndSendStateToReportAndResetMessageState()

      
    case TransferRequestDelete(id, bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc, fromHospitalCode, toHospitalCode, fromSchedule, message) =>
      
      log.debug(message)
      
      // Update hospital simulation summary
      simulationSummary = Some(
        HospitalHelper.updateHospitalSimulationSummary(
          hospitalCode = name,
          currentHss = simulationSummary,
          bedsWithIncomingPatientTypeSi = List(),
          bedsWithIncomingPatientTypeSc = List(),
          bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi,
          bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc))

      // Update current PRT simulatedHospitalState tracking `dynamic` incoming transferred patients
      simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForPrt(
        currentSimulatedHospitalStateOption = simulatedHospitalState,
        newStaticHospitalStateOption = currentHospitalState,
        bedsWithIncomingPatientTypeSi = List(),
        bedsWithIncomingPatientTypeSc = List(),
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc,
        patientTypeChangeFromScToSi = List(),
        patientTypeChangeFromSiToSc = List(),
        bedsWithTransferTypeOnlyChangePatientTypeSi = List(),
        bedsWithTransferTypeOnlyChangePatientTypeSc = List())

      val tRespDelete = TransferResponseDelete(correlationId = id, status = true, fromHospitalCode = fromHospitalCode, toHospitalCode = toHospitalCode, message = s"TransferRequestDelete for id ${id} accepted.")
      sender ! tRespDelete

      // Manage messageState
      messageState = messageState match {
        case Some(PrtMessageState(hs, tc, tu, td)) => Some(PrtMessageState(hs, tc, tu, Some(tRespDelete) ))
        case None => Some(PrtMessageState(hs = None, tc = None, tu = None, td = Some(tRespDelete)))
      }            
      
      // Check message state
      if (checkMessageStateCompleted(messageState)) requestNextDataAndSendStateToReportAndResetMessageState()

    
    case DataSetEmpty =>
      // TODO: provide aggregates to store in SIMULATION
      sender ! WorkCompleted("HosptialActorPrt", simulationSummary)

  }

}

/**
 * Contains latest states to manage data loop
 *
 * Unlike CdfMessageState in PrtMessageState `hs` is an option. Indeed we are guaranteed to have HospitalActorCdf and HospitalActorPrt
 * process the same schedule (a tuple of ELFIN). When HospitalActorCdf receives the new data it requests transfer create,update,delete
 * to HospitalActorPrt and HospitalActorPrt might receive these transfer requests before having received its new ELFIN data.
 * PrtMessageState creation process must support hs or tc or tu or td only data at creation time depending on the context of its first
 * initialisation.
 */
case class PrtMessageState(hs: Option[HospitalState], tc: Option[TransferResponseCreate], tu: Option[TransferResponseUpdate], td: Option[TransferResponseDelete]) {

}

