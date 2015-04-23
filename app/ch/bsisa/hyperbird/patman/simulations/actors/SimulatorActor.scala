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
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.actor.LocalActorRef
import akka.actor.PoisonPill
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper

/**
 * Simulation top coordinator actor
 */
class SimulatorActor(id: String, dateFrom: Date, dateTo: Date, cdfBedsNb: Int = 6, prtBedsNb: Int = 8, allBedsNb: Option[Int] = None, saturationThreshold: Option[Int] = None) extends Actor with ActorLogging {

  // Avoid Actor.context repetition i.e.: context.stop(self) => stop(self)
  import context._

  // Process parameters ===============================================
  val dateFromStr = DateUtil.hbDateFormat.format(dateFrom)
  val dateToStr = DateUtil.hbDateFormat.format(dateTo)  
  
  // Create children actors ===========================================
  //  val datasetActor = actorOf(Props(new DataSetActor), name = "dataSetActor")
  val shutdownCoordinatorActor = actorOf(Props(new ShutdownCoordinatorActor(simulationId = id)), name = "shutdownCoordinatorActor")

  // Report related actors
  val simulatedHospitalStateReportActor = actorOf(Props(new SimulatedHospitalStateReportActor(simulationId = id)), name = "simulatedHospitalStateReportActor")
  val transferReportActor = actorOf(Props(new TransferReportActor(simulationId = id)), name = "transferReportActor")
  
  // Hospital simulation actors
  val cdfHospitalActor = actorOf(Props(new HospitalActorCdf(name = "cdf", bedsNb = cdfBedsNb, simulatedHospitalStateReportActor = simulatedHospitalStateReportActor)), name = "cdfHospitalActor")
  val prtHospitalActor = actorOf(Props(new HospitalActorPrt(name = "prt", bedsNb = prtBedsNb, simulatedHospitalStateReportActor = simulatedHospitalStateReportActor)), name = "prtHospitalActor")

  val hospitalsActorRefMap: Map[String, ActorRef] = Map(HOSPITAL_CODE_CDF -> cdfHospitalActor, HOSPITAL_CODE_PRT -> prtHospitalActor)

  // Transfers simulation actor
  val transferActor = actorOf(Props(new TransferActor(hospitalsActorRefMap, transferReportActor)), name = "transferActor")

  
  // Load selected simulation data from database ====================== 
  // Database query
  val xqueryFileName = "hospitalStatesSelection.xq"
  val queryString = Option(s"dateFrom=${dateFromStr}&dateTo=${dateToStr}")

  // Query database HOSPITAL_STATE objects for requested time range
  val datasetAndTransferActorRefsPairFuture: Future[Option[ActorRef]] = XQueryWSHelper.runXQueryFile(xqueryFileName, queryString).map { response =>

    // hospitalStatesSelection.xq returns a list of XML ELFIN elements within a single MELFIN element.
    // ELFINs are sorted by schedule (IDENTIFIANT.DE), hospital code (CARACTERISTIQUE/FRACTION/L[POS='1']/C[POS='1']/string()) ascending
    val melfinWrappedBody = response.body.mkString
    // Convert ELFIN XML to ELFIN objects 
    val elfins: Seq[ELFIN] = ElfinFormat.elfinsFromXml(scala.xml.XML.loadString(melfinWrappedBody))

    // Create datasetActor with dataset iterator for the current simulation. 
    // Note: DataSetActor abstraction could scale to a cluster of actors if necessary
    val datasetActor = actorOf(Props(new DataSetActor(elfins.iterator)), name = "dataSetActor")

    Some(datasetActor)
  }.recover {
    case e: Throwable => {
      log.error(s"XQueryWSHelper.runXQueryFile failed with exception: ${e}")
      log.warning(s"Stopping SimulatorActor named: ${self.path.name}")
      stop(self)
      None
    }
  }

  // Initialisation process can block
  val datasetActorOption = Await.result(datasetAndTransferActorRefsPairFuture, 1 minutes) // scala.concurrent.duration._
  val datasetActor = datasetActorOption.get

  // Starts analysis by requesting the first record
  datasetActor ! HospitalStatesRequestInit

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
          log.info(s"Received NextHospitalStatesRequest fromHospital = ${fromHospital}")
          pendingCdfNextHospitalStatesRequest = true
        case HOSPITAL_CODE_PRT =>
          log.info(s"Received NextHospitalStatesRequest fromHospital = ${fromHospital}")
          pendingPrtNextHospitalStatesRequest = true
      }
      if (pendingCdfNextHospitalStatesRequest && pendingPrtNextHospitalStatesRequest) {

        // Reset pending states to false
        pendingCdfNextHospitalStatesRequest = false
        pendingPrtNextHospitalStatesRequest = false

        // Request next data for next cdf and prt schedule
        datasetActor ! HospitalStatesRequest
      } else {
        if (pendingPrtNextHospitalStatesRequest) {
          log.info(s"Waiting for ${HOSPITAL_CODE_CDF} NextHospitalStatesRequest")
        } else {
          log.info(s"Waiting for ${HOSPITAL_CODE_PRT} NextHospitalStatesRequest")
        }
      }
    }

    // When all data has been delivered by DataSetActor notify children and wait for WorkCompleted
    case DataSetEmpty =>
      log.info(s"Notifying ${children.size} children DataSetEmpty")
      for (child <- children) {
        child ! DataSetEmpty
      }

    // When all WorkCompleted messages have been received we should receive the StopSimulationRequest
    case WorkCompleted(message, hssOpt) =>
      // Termination size is minus 1 for ShutdownCoordinatorActor itself not responding to DataSetEmpty message.
      shutdownCoordinatorActor ! ShutdownSignal(message = message, terminationSize = children.size - 1, hssOpt)

    // Shuts down the simulation 
    case StopSimulationRequest(reason) =>
      log.info(s"Simulation ${self.path.name} has been requested to stop for reason: $reason")
      stop(self)

  }

}
