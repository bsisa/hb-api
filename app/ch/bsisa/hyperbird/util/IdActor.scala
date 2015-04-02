package ch.bsisa.hyperbird.util;

import play.libs.Akka
import akka.actor.Actor
import akka.actor.Props
import java.util.Date
import akka.actor.ActorLogging

/**
 * We assume no more than 1000 ids are requested per second.
 * 
 * This is a weak assumption for generic ids generator but 
 * is acceptable with the current system business usage. 
 * 
 * Due to ids backward compatibility this currently is the  
 * best option. 
 * 
 * Moving to UUID or pseudo nano sec, are evaluated.
 */
class IdActor extends Actor {

	var counter = 0
  
	def receive = {
	  case msg: String =>
	    counter = if (counter < 999) counter + 1 else 0
	    val paddedCounter = f"${counter}%03.0f"
	    val newId = "G" + DateUtil.elfinUniqueIdDateFormat.format(new Date())
	    val newIdWithCounter = newId.substring(0, newId.size-3).concat(paddedCounter)
	    sender ! newIdWithCounter
	}
	
}



