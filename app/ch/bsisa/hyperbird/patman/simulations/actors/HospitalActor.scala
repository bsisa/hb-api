package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{Actor,ActorLogging}
import ch.bsisa.hyperbird.patman.simulations.messages.HospitalState
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.patman.simulations.messages.HospitalStatesRequest
import ch.bsisa.hyperbird.patman.simulations.messages.NextHospitalStatesRequest

class HospitalActor(name : String, bedsNb : Int) extends Actor with ActorLogging {
  
	def receive = {
	  case hospitalState: HospitalState => 
	    log.info(s"HospitalActor(${name}) received new hospitalState schedule ${hospitalState.elfin.IDENTIFIANT.get.DE.get}")
	    val hospital = HospitalHelper.toHospital(hospitalState.elfin)
	    log.info("hospital: " + hospital)
	    // Check state received hospital id matches our name otherwise cancel simulation!
	    sender ! NextHospitalStatesRequest(name)
	}
	
}



