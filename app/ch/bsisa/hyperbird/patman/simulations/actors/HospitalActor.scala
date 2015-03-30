package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{Actor,ActorLogging}
import ch.bsisa.hyperbird.patman.simulations.messages.HospitalState
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.patman.simulations.messages.HospitalStatesRequest
import ch.bsisa.hyperbird.patman.simulations.messages.NextHospitalStatesRequest
import ch.bsisa.hyperbird.patman.simulations.model.Hospital

class HospitalActor(name : String, bedsNb : Int) extends Actor with ActorLogging {
  
	var previousHospitalState : Option[Hospital] = None 
	var currentHospitalState : Option[Hospital] = None
  
	def receive = {
	  case hospitalState: HospitalState => 
	    log.info(s"HospitalActor(${name}) received new hospitalState schedule ${hospitalState.elfin.IDENTIFIANT.get.DE.get}")
	    val hospital = HospitalHelper.toHospital(hospitalState.elfin)
	    log.info(s"============================== $name - start ==============================")
	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
	    log.info(s"------------------------------ $name --------------------------------------")
	    // Roll states
	    previousHospitalState = currentHospitalState
	    currentHospitalState = Some(hospital)
	    log.info(s"$name> previousHospitalState: " + previousHospitalState)
	    log.info(s"$name> currentHospitalState: " + currentHospitalState)
	    log.info(s"------------------------------ $name --------------------------------------")
	    log.info(s"$name> BedsWithIncomingPatient: " + HospitalHelper.getBedsWithIncomingPatient(previousHospitalState, currentHospitalState) )
	    log.info(s"$name> BedsWithOutgoingPatient: " + HospitalHelper.getBedsWithOutgoingPatient(previousHospitalState, currentHospitalState) )
	    log.info(s"============================== $name - end   ==============================")
	    
	    
	    // Check state received hospital id matches our name otherwise cancel simulation!
	    sender ! NextHospitalStatesRequest(name)
	}
	
}



