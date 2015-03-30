package ch.bsisa.hyperbird.patman.simulations.actors

import play.libs.Akka
import akka.actor.{ Actor, ActorRef, ActorLogging }
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

  // Avoid Actor.context repetition i.e.: context.stop(self) => stop(self)
  import context._

  // Create children actors
  val datasetActor = actorOf(Props(new DataSetActor), name = "dataSetActor")
  val cdfHospitalActor = actorOf(Props(new HospitalActorCdf(name = "cdf", bedsNb = cdfBedsNb)), name = "cdfHospitalActor")
  val prtHospitalActor = actorOf(Props(new HospitalActorPrt(name = "prt", bedsNb = prtBedsNb)), name = "prtHospitalActor")

  val hospitalsActorRefMap : Map[String, ActorRef] = Map(HOSPITAL_CODE_CDF -> cdfHospitalActor, HOSPITAL_CODE_PRT -> prtHospitalActor)
  
  val transferActor = actorOf(Props(new TransferActor(hospitalsActorRefMap, datasetActor)), name = "transferActor")

  // Process parameters
  val dateFromStr = DateUtil.hbDateFormat.format(dateFrom)
  val dateToStr = DateUtil.hbDateFormat.format(dateTo)

  // Database query
  val xqueryFileName = "hospitalStatesSelection.xq"
  val queryString = Option(s"dateFrom=${dateFromStr}&dateTo=${dateToStr}")

  // Query database HOSPITAL_STATE objects for requested time range
  val futureElfins = XQueryWSHelper.runXQueryFile(xqueryFileName, queryString).map { response =>

    // hospitalStatesSelection.xq returns a list of XML ELFIN elements within a single MELFIN element.
    // ELFINs are sorted by schedule (IDENTIFIANT.DE), hospital code (CARACTERISTIQUE/FRACTION/L[POS='1']/C[POS='1']/string()) ascending
    val melfinWrappedBody = response.body.mkString
    // Convert ELFIN XML to ELFIN objects 
    val elfins: Seq[ELFIN] = ElfinFormat.elfinsFromXml(scala.xml.XML.loadString(melfinWrappedBody))

    // Provide dataset to the dataset actor for the current simulation. (Note: DataSetActor abstraction could scale to a cluster of actors if necessary)
    datasetActor ! DataSet(elfins)

  }.recover {
    case e: Throwable => {
      log.error(s"XQueryWSHelper.runXQueryFile failed with exception: ${e}")
      log.warning(s"Stopping SimulatorActor named: ${self.path.name}")
      stop(self)
    }
  }

  // Mutable states enabling to join cdf and prt request for data 
  var pendingCdfNextHospitalStatesRequest = false
  var pendingPrtNextHospitalStatesRequest = false

  /**
   * Process messages
   */
  def receive = {

    // Data delivered by DataSetActor
    case HospitalStatesResponse(cdfHospitalState, prtHospitalState, message) =>
      log.info(s"HospitalStatesResponse: ${message}")
      cdfHospitalActor ! HospitalState(cdfHospitalState, transferActor)
      prtHospitalActor ! HospitalState(prtHospitalState, transferActor)
      
    // Request for next data from DataSetActor, waits to join both cdf and prt identical requests.
    case NextHospitalStatesRequest(fromHospital) => {
      fromHospital match {
        case HOSPITAL_CODE_CDF => 
          log.info("Received NextHospitalStatesRequest fromHospital = ${fromHospital}")
          pendingCdfNextHospitalStatesRequest = true
        case HOSPITAL_CODE_PRT => 
          log.info("Received NextHospitalStatesRequest fromHospital = ${fromHospital}")
          pendingPrtNextHospitalStatesRequest = true
      }
      if (pendingCdfNextHospitalStatesRequest && pendingPrtNextHospitalStatesRequest) {
        // Reset pending states to false
        pendingCdfNextHospitalStatesRequest = false
        pendingPrtNextHospitalStatesRequest = false
        // Request next data for next cdf and prt schedule
        datasetActor ! HospitalStatesRequest
      }
    }

    // When all data has been delivered by DataSetActor
    case DataSetEmpty =>
      log.info(s"DataSetEmpty: stoping simulation ${self.path.name}")
      stop(self)

    case StopSimulationRequest(reason) => 
      log.error(s"Simulation ${self.path.name} has been requested to stop for reason: $reason")
      stop(self)
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

}
