package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{Actor,ActorLogging}
import ch.bsisa.hyperbird.patman.simulations.messages.HospitalState

class HospitalActor(name : String) extends Actor with ActorLogging {

	def receive = {
	  case hospitalState: HospitalState => 
	    log.info(s"HospitalActor(${name}) received new hospitalState '$hospitalState' from ${sender.path.name} of CLASSE = ${hospitalState.elfin.CLASSE} waiting for 5sec!!!")
	    Thread.sleep(5000)
	    // Check state received hospital id matches our name otherwise cancel simulation!
	    sender ! s"received hospitalState of CLASSE = ${hospitalState.elfin.CLASSE}"
	}
	
}



