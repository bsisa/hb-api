package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{Actor,ActorLogging}

class TransferActor extends Actor with ActorLogging {

  
  // Listen to request for transfer
  // Request hospital(s) to know whether the transfer is possible
  // Sends transfer approved / refused confirmation to requester
  // Perform transfer
  
    def receive = {
    case msg: String =>
      val simulatorName = sender.path.name
      log.info(s"TransferActor received message '$msg'. WARNING: STOPPING ${simulatorName} for test workflow only!!!")
      
      sender ! "Stop"
      
  }
  
  
  
}