package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{ Actor, ActorLogging }
import ch.bsisa.hyperbird.patman.simulations.messages.{ DataSet, DataSetEmpty, HospitalStatesRequest, HospitalStatesResponse }
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.util.DateUtil
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.patman.simulations.messages.DataSetUpdateRequest
import ch.bsisa.hyperbird.patman.simulations.messages.DataSetUpdateResponse
import ch.bsisa.hyperbird.patman.simulations.model.Bed

class DataSetActor extends Actor with ActorLogging {

  var initialised: Boolean = false
  var dataSetIterator: Iterator[ELFIN] = Iterator()

  def receive = {
    case DataSet(hospitalStates) =>
      setDataSet(hospitalStates)

      // We start analysis at a defined start schedule
      // Check searchFirstStartSchedule() for rules that apply
      val firstStartScheduleElfin = searchFirstStartSchedule(dataSetIterator)
      firstStartScheduleElfin match {
        case Some(elfin) =>
          log.info("Found start schedule")
          // loop schedule by schedule
          getNextHospitalStates(elfin, dataSetIterator) match {
            case Some((cdfHospitalState, prtHospitalState)) =>
              log.info("Obtained hospital states pair corresponding to start schedule")
              sender ! HospitalStatesResponse(cdfHospitalState, prtHospitalState, "Dataset initialisation successful, provide first record.")
            case None =>
              // No data stop simulation
              log.warning("Could not obtain hospital states pair corresponding to start schedule")
              log.info(s"Notify dataset empty for simulation ${self.path.name}")
              sender ! DataSetEmpty
          }
        case None =>
          // No data stop simulation
          log.warning("Could not obtain any hospital states to process")
          log.info(s"Notify dataset empty for simulation ${self.path.name}")
          sender ! DataSetEmpty
      }
    case HospitalStatesRequest =>
      log.info("HospitalStatesRequest")
      getNextHospitalStates(dataSetIterator) match {
        case Some((cdfHospitalState, prtHospitalState)) => sender ! HospitalStatesResponse(cdfHospitalState, prtHospitalState, "Provide next record.")
        case None => sender ! DataSetEmpty
      }
    case DataSetUpdateRequest(id, transferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule) =>
      log.info(s"DataSetUpdateRequest id ${id} for ${transferredSiBeds}, from ${fromHospitalCode} to ${toHospitalCode} from schedule: ${fromSchedule}")
      // Do the job
      // =========================================================================================================
      // TODO: Move all `transferredSiBeds` from `fromHospitalCode` to `toHospitalCode` from `fromSchedule` on.
      // =========================================================================================================
      log.info(">>>>>>> START DATASET UPDATE     <<<<<<<<<< ")
      updateDataset(transferredSiBeds, fromHospitalCode, toHospitalCode)
      log.info(">>>>>>> DATASET UPDATE COMPLETED <<<<<<<<<< ")      
      // =========================================================================================================
      // Send the response
      sender ! DataSetUpdateResponse(id, DATASET_UPDATE_RESPONSE_SUCCESS, transferredSiBeds, fromHospitalCode, toHospitalCode, fromSchedule)
  }

  def setDataSet(dataSet: Seq[ELFIN]): Unit = {
    if (!initialised) {
      initialised = true
      dataSetIterator = dataSet.iterator
    } else {
      log.error(s"DataSetActor.dataSetIterator should be initialised only once per simulation! SimulatorActor = ${sender.path.name} tried to re-initialise.")
    }
  }

  /**
   * Note: No need for fromSchedule. We deal with an iterator thus only fromSchedules are available.
   * Current test implementation does only a copy of the existing data without modifying them.
   */
  def updateDataset(transferredSiBeds: List[Bed], fromHospitalCode: String, toHospitalCode: String): Unit = {

    def doIt(updatedList: List[ELFIN]): List[ELFIN] = {
      getNextHospitalStates(dataSetIterator) match {
        case Some((cdfHospitalState, prtHospitalState)) => doIt(prtHospitalState :: cdfHospitalState :: updatedList)
        case None => updatedList
      }
    }

    // doIt build the list in reverse order preserving ordering
    val reversedUpdatedElfins = doIt(Nil)
    //log.info(s">>>>>> ${reversedUpdatedElfins}");
    val updatedElfins: List[ELFIN] = reversedUpdatedElfins.reverse
    //log.info(s">>>>>> ${updatedElfins}");
    
    dataSetIterator = updatedElfins.iterator
    
    //    val updatedDataSetIterator = for (elfin <- dataSetIterator) yield {
    //      if (getHospitalCode(elfin) == fromHospitalCode) {
    //
    //      } else {
    //
    //      }
    //    }
    //Iterator()//TODO: REMOVE.
  }

  def validateSameSchedule(elfin1: ELFIN, elfin2: ELFIN): Boolean = {
    val date1 = DateUtil.getIsoDateFormatterWithoutTz.parse(elfin1.IDENTIFIANT.get.DE.get)
    val hms1 = DateUtil.getHourMinuteSecond(date1)
    val date2 = DateUtil.getIsoDateFormatterWithoutTz.parse(elfin2.IDENTIFIANT.get.DE.get)
    val hms2 = DateUtil.getHourMinuteSecond(date2)
    (hms1._1 == hms2._1 && hms1._2 == hms2._2 && hms1._3 == hms2._3)
  }

  def validateExpectedHospitalCodes(cdfHospitalState: ELFIN, prtHospitalState: ELFIN): Boolean = {
    val expectedCdfCode = getHospitalCode(cdfHospitalState)
    val expectedPrtCode = getHospitalCode(prtHospitalState)
    (expectedCdfCode == HOSPITAL_CODE_CDF && expectedPrtCode == HOSPITAL_CODE_PRT)
  }

  def getNextHospitalStates(cdfHospitalState: ELFIN, elfinsIt: Iterator[ELFIN]): Option[(ELFIN, ELFIN)] = {
    if (elfinsIt.hasNext) {
      // Get associated HS
      val prtHospitalState = elfinsIt.next
      //checkSameSchedule
      if (validateSameSchedule(cdfHospitalState, prtHospitalState) && validateExpectedHospitalCodes(cdfHospitalState, prtHospitalState)) {
        Option(cdfHospitalState, prtHospitalState)
      } else {
        None
      }
    } else {
      log.info(s"No corresponding CDF HospitalState for ${cdfHospitalState.IDENTIFIANT.get}")
      None
    }
  }

  def getNextHospitalStates(elfinsIt: Iterator[ELFIN]): Option[(ELFIN, ELFIN)] = {
    if (elfinsIt.hasNext) {
      val cdfHospitalState = elfinsIt.next
      if (elfinsIt.hasNext) {
        // Get associated HS
        val prtHospitalState = elfinsIt.next
        //checkSameSchedule
        if (validateSameSchedule(cdfHospitalState, prtHospitalState) && validateExpectedHospitalCodes(cdfHospitalState, prtHospitalState)) {
          Option(cdfHospitalState, prtHospitalState)
        } else {
          None
        }
      } else {
        log.info(s"No corresponding CDF HospitalState for ${cdfHospitalState.IDENTIFIANT.get}")
        None
      }
    } else {
      log.info(s"No more data")
      None
    }
  }

  /**
   * Returns true if `elfin` is a `08:00:00` o'clock schedule for `HOSPITAL_CODE_CDF`
   */
  def isFirstStartSchedule(elfin: ELFIN): Boolean = {

    val elfinSchedule = DateUtil.getIsoDateFormatterWithoutTz.parse(elfin.IDENTIFIANT.get.DE.get)
    val (elfinScheduleHour, elfinScheduleMinutes, elfinScheduleSeconds) = DateUtil.getHourMinuteSecond(elfinSchedule)
    val hospitalCode = getHospitalCode(elfin)

    (hospitalCode == HOSPITAL_CODE_CDF) && (elfinScheduleHour == 8 && elfinScheduleMinutes == 0 && elfinScheduleSeconds == 0)
  }

  /**
   * Checks `elfinsIt` for first elfin satisfying isFirstSchedule test
   */
  def searchFirstStartSchedule(elfinsIt: Iterator[ELFIN]): Option[ELFIN] = {
    if (elfinsIt.hasNext) {
      val elfin = elfinsIt.next
      if (isFirstStartSchedule(elfin)) Option(elfin) else searchFirstStartSchedule(elfinsIt)
    } else {
      None
    }
  }

  def getHospitalCode(elfin: ELFIN): String = getMixedContent(elfin.CARACTERISTIQUE.get.FRACTION.get.L(0).C(0).mixed)

}



