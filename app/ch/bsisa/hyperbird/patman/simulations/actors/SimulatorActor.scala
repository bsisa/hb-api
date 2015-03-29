package ch.bsisa.hyperbird.patman.simulations.actors

import play.libs.Akka
import akka.actor.{ Actor, ActorLogging }
import akka.actor.Props
import java.util.Date
import ch.bsisa.hyperbird.Implicits.{ dbConfig, collectionsConfig }
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.patman.simulations.messages.HospitalState
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.util.DateUtil

class SimulatorActor(dateFrom: Date, dateTo: Date, cdfBedsNb: Int = 6, prtBedsNb: Int = 8, allBedsNb: Option[Int] = None, saturationThreshold: Option[Int] = None) extends Actor with ActorLogging {

  import context._

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
    for ((elfin, i) <- elfins.zipWithIndex) {
    	log.info(s"""ELFIN: DE = ${elfin.IDENTIFIANT.get.DE.get}, abbrev. =  ${elfin.CARACTERISTIQUE.get.FRACTION.get.L(0).C(0).mixed.mkString}""")
    }

    val testCdfElfin = new ELFIN(Id = "testId", ID_G = "testID_G", CLASSE = "TEST_CDF", NATURE = "DOCUMENT")
    val testPrtElfin = new ELFIN(Id = "testId", ID_G = "testID_G", CLASSE = "TEST_PRT", NATURE = "DOCUMENT")

    cdfHospitalActor ! HospitalState(testCdfElfin)
    prtHospitalActor ! HospitalState(testPrtElfin)

  }.recover {
    case e: Throwable => {
      log.error(s"XQueryWSHelper.runXQueryFile failed with exception: ${e}")
      log.warning(s"Stopping SimulatorActor named: ${self.path.name}")
      stop(self)
    }
  }

  // TODO: remove. Test only stop condition.
  var i = 0

  def receive = {
    case msg: String =>
      log.info(s"SimulatorActor(dateFrom = ${dateFrom}, dateTo = ${dateTo}) received message '$msg'")
      i = i + 1
      if (i == 2) transferActor ! "Hello, received two messages as expected."

      if (msg == "Stop") {
        log.info(s"SimulatorActor(dateFrom = ${dateFrom}, dateTo = ${dateTo}) going to stop...")
        // Stops this actor and all its supervised children
        stop(self)
      }
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