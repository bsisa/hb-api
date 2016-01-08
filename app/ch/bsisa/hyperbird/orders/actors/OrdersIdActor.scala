package ch.bsisa.hyperbird.orders.actors

import ch.bsisa.hyperbird.orders.messages.OrderIdRequest
import ch.bsisa.hyperbird.orders.messages.OrderIdResponse
import ch.bsisa.hyperbird.orders.messages.OrdersMaxValueInit
import akka.actor.Actor
import akka.actor.ActorLogging 
import akka.actor.actorRef2Scala


/**
 * Orders id service providing fast thread safe unique id number. 
 */
class OrdersIdActor extends Actor with ActorLogging {

  var initialised = false
	var maxValue = 0
  
	def receive = {
	  case req : OrderIdRequest =>
      if (initialised) {
        val newMaxValue = maxValue+1
        maxValue = newMaxValue
  	    sender ! OrderIdResponse(Some(newMaxValue))
      } else {
        sender ! OrderIdResponse(None)
      }
    case init : OrdersMaxValueInit => 
      maxValue = init.maxValue
      initialised = true
      log.info(s"Initialisation succeeded with maxValue = ${maxValue}")
	}
	
}

