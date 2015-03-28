package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.Actor

class HospitalActor extends Actor {

	def receive = {
	  case msg: String => 
	    println(s"HospitalActor received message '$msg'")
	}
	
}



