package ch.bsisa.hyperbird.patman.simulations.actors

import play.libs.Akka
import akka.actor.{ Actor, ActorLogging }
import akka.actor.Props
import java.util.Date
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.patman.simulations.messages.HospitalState

class SimulatorActor(dateFrom: Date, dateTo: Date) extends Actor with ActorLogging {
  import context._
  val cdfHospitalActor = actorOf(Props(new HospitalActor("cdf")), name = "cdfHospitalActor")
  val prtHospitalActor = actorOf(Props(new HospitalActor("prt")), name = "prtHospitalActor")
  val transferActor = actorOf(Props[TransferActor], name = "transferActor")

  val testCdfElfin = new ELFIN(Id = "testId", ID_G = "testID_G", CLASSE = "TEST_CDF", NATURE = "DOCUMENT")
  val testPrtElfin = new ELFIN(Id = "testId", ID_G = "testID_G", CLASSE = "TEST_PRT", NATURE = "DOCUMENT")

  cdfHospitalActor ! HospitalState(testCdfElfin)
  prtHospitalActor ! HospitalState(testPrtElfin)

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

  // 1. Query database HOSPITAL_STATE objects for data according to provided constructor parameters (date from, date to, ...)

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