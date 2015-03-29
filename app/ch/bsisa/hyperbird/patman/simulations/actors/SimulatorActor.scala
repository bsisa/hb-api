package ch.bsisa.hyperbird.patman.simulations.actors

import play.libs.Akka
import akka.actor.{ Actor, ActorLogging }
import akka.actor.Props
import java.util.Date
import java.util.Calendar
import ch.bsisa.hyperbird.Implicits.{ dbConfig, collectionsConfig }
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.util.DateUtil

class SimulatorActor(dateFrom: Date, dateTo: Date, cdfBedsNb: Int = 6, prtBedsNb: Int = 8, allBedsNb: Option[Int] = None, saturationThreshold: Option[Int] = None) extends Actor with ActorLogging {

  import context._

  val datasetActor = actorOf(Props(new DataSetActor), name = "dataSetActor")
  val cdfHospitalActor = actorOf(Props(new HospitalActor("cdf")), name = "cdfHospitalActor")
  val prtHospitalActor = actorOf(Props(new HospitalActor("prt")), name = "prtHospitalActor")
  val transferActor = actorOf(Props[TransferActor], name = "transferActor")

  // Process parameters
  val dateFromStr = DateUtil.hbDateFormat.format(dateFrom)
  val dateToStr = DateUtil.hbDateFormat.format(dateTo)

  val xqueryFileName = "hospitalStatesSelection.xq"
  val queryString = Option(s"dateFrom=${dateFromStr}&dateTo=${dateToStr}")

  // 1. Query database HOSPITAL_STATE objects for data according to provided constructor parameters (date from, date to, ...)
  val futureElfins = XQueryWSHelper.runXQueryFile(xqueryFileName, queryString).map { response =>
    log.debug(s">>>> runXQueryFile: Result of type ${response.ahcResponse.getContentType} received")
    // hospitalStatesSelection.xq returns a list of XML ELFIN elements within a single MELFIN element.
    val melfinWrappedBody = response.body.mkString
    val elfins = ElfinFormat.elfinsFromXml(scala.xml.XML.loadString(melfinWrappedBody))

    // Provided dataset to the dataset actor for the current simulation
    // DataSetActor abstraction could scale to a cluster of actors if necessary.
    datasetActor ! DataSet(elfins)

//    val elfinsIt: Iterator[ELFIN] = elfins.iterator
//
//    // We start analysis at a defined start schedule
//    // Check searchFirstStartSchedule() for rules that apply
//    val firstStartScheduleElfin = searchFirstStartSchedule(elfinsIt)
//    firstStartScheduleElfin match {
//      case Some(elfin) =>
//        log.info("Found start schedule")
//        // loop schedule by schedule
//        getNextHospitalStates(elfin, elfinsIt) match {
//          case Some((cdfHospitalState, prfHospitalState)) =>
//            log.info("Obtained hospital states pair corresponding to start schedule")
//            cdfHospitalActor ! cdfHospitalState
//            prtHospitalActor ! prfHospitalState
//          case None =>
//            // No data stop simulation
//            log.warning("Could not obtain hospital states pair corresponding to start schedule")
//            log.info(s"Stopping simulation ${self.path.name}")
//            stop(self)
//        }
//      case None =>
//        // No data stop simulation
//        log.warning("Could not obtain any hospital states to process")
//        log.info(s"Stopping simulation ${self.path.name}")
//        stop(self)
//    }

    //      for ((elfin, i) <- elfins.zipWithIndex) {
    //        val isFirstScheduleStr = if (isFirstSchedule(elfin)) " >>>> IS FIRST SCHEDULE !!! " else ""
    //        log.info(s"""ELFIN: DE = ${elfin.IDENTIFIANT.get.DE.get}, abbrev. =  ${getMixedContent(elfin.CARACTERISTIQUE.get.FRACTION.get.L(0).C(0).mixed)} ${isFirstScheduleStr}""")
    //      }

    //    val testCdfElfin = new ELFIN(Id = "testId", ID_G = "testID_G", CLASSE = "TEST_CDF", NATURE = "DOCUMENT")
    //    val testPrtElfin = new ELFIN(Id = "testId", ID_G = "testID_G", CLASSE = "TEST_PRT", NATURE = "DOCUMENT")
    //
    //    cdfHospitalActor ! HospitalState(testCdfElfin)
    //    prtHospitalActor ! HospitalState(testPrtElfin)

  }.recover {
    case e: Throwable => {
      log.error(s"XQueryWSHelper.runXQueryFile failed with exception: ${e}")
      log.warning(s"Stopping SimulatorActor named: ${self.path.name}")
      stop(self)
    }
  }

  // TODO: remove. Test only stop condition.
  //var i = 0

  def receive = {

//    case DataSetReady =>
//      log.info(s"DataSetReady message received, start processing.")
//      datasetActor ! HospitalStatesRequest
    case HospitalStatesResponse(cdfHospitalState, prtHospitalState, message) =>
      log.info(s"${message}")
      // Trigger next iteration for test
      datasetActor ! HospitalStatesRequest
    case DataSetEmpty => 
      log.info("Notified `DataSetEmpty`, stoping simulation.")
      stop(self)

    //    case msg: String =>
    //      log.info(s"SimulatorActor(dateFrom = ${dateFrom}, dateTo = ${dateTo}) received message '$msg'")
    //      i = i + 1
    //      if (i == 2) transferActor ! "Hello, received two messages as expected."
    //
    //      if (msg == "Stop") {
    //        log.info(s"SimulatorActor(dateFrom = ${dateFrom}, dateTo = ${dateTo}) going to stop...")
    //        // Stops this actor and all its supervised children
    //        stop(self)
    //      }
  }

  // 2. Dispatch HOSPITAL_STATE objects according to hospital identifier, for each given time t

  // 3. Proceed with response, waiting to merge results from all hospitals for a given time t 

  // 4. Once a response for a given time t has been completed:
  //    a) Send the result to the SimulationResultManagerActor
  //    b) proceed with t+1 if available 
  //    or 
  //    c) shutdown the simulation if:
  //       I)  All objects have been processed 
  //           and
  //       II) SimulationResultManagerActor has sent `work completed` message

//  /**
//   * Returns true if `elfin` is a `08:00:00` o'clock schedule for `HOSPITAL_CODE_CDF`
//   */
//  def isFirstStartSchedule(elfin: ELFIN): Boolean = {
//
//    val elfinSchedule = DateUtil.isoWithoutTzDateFormat.parse(elfin.IDENTIFIANT.get.DE.get)
//
//    val calendar = Calendar.getInstance()
//    calendar.setTime(elfinSchedule)
//    val elfinScheduleHour = calendar.get(Calendar.HOUR_OF_DAY) // Hour in 24h format
//    val elfinScheduleMinutes = calendar.get(Calendar.MINUTE)
//    val elfinScheduleSeconds = calendar.get(Calendar.SECOND)
//    val hospitalCode = getMixedContent(elfin.CARACTERISTIQUE.get.FRACTION.get.L(0).C(0).mixed)
//
//    (hospitalCode == HOSPITAL_CODE_CDF) && (elfinScheduleHour == 8 && elfinScheduleMinutes == 0 && elfinScheduleSeconds == 0)
//  }
//
//  /**
//   * Checks `elfinsIt` for first elfin satisfying isFirstSchedule test
//   */
//  def searchFirstStartSchedule(elfinsIt: Iterator[ELFIN]): Option[ELFIN] = {
//    if (elfinsIt.hasNext) {
//      val elfin = elfinsIt.next
//      if (isFirstStartSchedule(elfin)) Option(elfin) else searchFirstStartSchedule(elfinsIt)
//    } else {
//      None
//    }
//  }

  // TODO: review: no need of loop only need to provide a pair of HospitalState on demand
  // of feedback messages...
  //  def processData(firstElfin: ELFIN, elfinsIt: Iterator[ELFIN]): Unit = {
  // 
  //    def go(cdfHospitalState : ELFIN) : Unit = {
  //    	if (elfinsIt.hasNext) { 
  //    	  // Get associated HS
  //    	  val prtHospitalState = elfinsIt.next
  //    	  //checkSameSchedule
  //    	  if (validateSameSchedule(cdfHospitalState,prtHospitalState) && validateExpectedHospitalCodes(cdfHospitalState,prtHospitalState)) {
  //    	    // DO THE JOB for the successful pair...
  //    	  }
  //    	} else {
  //    	  log.info(s"No corresponding CDF HospitalState for ${cdfHospitalState.IDENTIFIANT.get}")
  //    	}
  //    }
  //
  //    go(firstElfin)
  //  }

//  def validateExpectedHospitalCodes(cdfHospitalState: ELFIN, prtHospitalState: ELFIN): Boolean = {
//    val expectedCdfCode = getMixedContent(cdfHospitalState.CARACTERISTIQUE.get.FRACTION.get.L(0).C(0).mixed)
//    val expectedPrtCode = getMixedContent(prtHospitalState.CARACTERISTIQUE.get.FRACTION.get.L(0).C(0).mixed)
//    (expectedCdfCode == HOSPITAL_CODE_CDF && expectedPrtCode == HOSPITAL_CODE_PRT)
//  }

//  def validateSameSchedule(elfin1: ELFIN, elfin2: ELFIN): Boolean = {
//    val date1 = DateUtil.isoWithoutTzDateFormat.parse(elfin1.IDENTIFIANT.get.DE.get)
//    val hms1 = DateUtil.getHourMinuteSecond(date1)
//    val date2 = DateUtil.isoWithoutTzDateFormat.parse(elfin2.IDENTIFIANT.get.DE.get)
//    val hms2 = DateUtil.getHourMinuteSecond(date2)
//    (hms1._1 == hms2._1 && hms1._2 == hms2._2 && hms1._3 == hms2._3)
//  }

//  def getHourMinuteSecond(date: Date): (Int, Int, Int) = {
//    val calendar = Calendar.getInstance()
//    calendar.setTime(date)
//    // Hour in 24h format
//    (calendar.get(Calendar.HOUR_OF_DAY), calendar.get(Calendar.MINUTE), calendar.get(Calendar.SECOND))
//  }

//  def getNextHospitalStates(cdfHospitalState: ELFIN, elfinsIt: Iterator[ELFIN]): Option[(ELFIN, ELFIN)] = {
//    if (elfinsIt.hasNext) {
//      // Get associated HS
//      val prtHospitalState = elfinsIt.next
//      //checkSameSchedule
//      if (validateSameSchedule(cdfHospitalState, prtHospitalState) && validateExpectedHospitalCodes(cdfHospitalState, prtHospitalState)) {
//        Option(cdfHospitalState, prtHospitalState)
//      } else {
//        None
//      }
//    } else {
//      log.info(s"No corresponding CDF HospitalState for ${cdfHospitalState.IDENTIFIANT.get}")
//      None
//    }
//  }

}