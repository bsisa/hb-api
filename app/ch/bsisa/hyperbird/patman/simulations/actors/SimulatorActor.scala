package ch.bsisa.hyperbird.patman.simulations.actors

import play.libs.Akka
import akka.actor.{Actor, ActorLogging}
import akka.actor.Props
import java.util.Date

class SimulatorActor(dateFrom : Date, dateTo : Date) extends Actor with ActorLogging {

  def receive = {
    case msg: String =>
      log.info(s"SimulatorActor(dateFrom = ${dateFrom}, dateTo = ${dateTo}) received message '$msg'")
      if (msg == "Stop") {
        log.info(s"SimulatorActor(dateFrom = ${dateFrom}, dateTo = ${dateTo}) going to stop...")
        // Stops this actor and all its supervised children
        context.stop(self)
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