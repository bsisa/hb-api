package ch.bsisa.hyperbird.util;

import play.libs.Akka
import akka.actor.Actor
import akka.actor.Props

class IdActor extends Actor {

	def receive = {
	  case msg: String => 
	    println(s"IdActor received message '$msg'")
	}
	
}



