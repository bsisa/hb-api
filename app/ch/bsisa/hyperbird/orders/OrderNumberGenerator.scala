package ch.bsisa.hyperbird.orders

import ch.bsisa.hyperbird.orders.actors.OrdersIdActor
import ch.bsisa.hyperbird.orders.messages._

import play.libs.Akka
import play.api.Logger

import akka.actor.Props
import akka.pattern.ask

import scala.concurrent.duration._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Simple orders service providing sequential orders number using maximum value
 * from database at start up. Then no database query are perform for this purpose
 * the ordersIdActor holds current max orders number state in a thread safe way. 
 * 
 * @author Patrick Refondini
 */
object OrderNumberGenerator {
  
  val ordersIdActor = Akka.system.actorSelection("akka://application/user/ordersIdActor")
  
  /**
   * Provides next order number.
   */
  def getNewOrderNumber : Future[Option[Int]] = {
    
    // Warning: sending object OrderIdRequest fails while OrderIdRequest() classe instance works.
    val futureResponse = (ordersIdActor.ask(OrderIdRequest())(5 seconds)).mapTo[OrderIdResponse]
    
    futureResponse.map { response  => 
      response.id match {
        case Some(number) => response.id  
        case None => 
          Logger.warn("Orders number generator service not available.")
          response.id
      } 
      
    }

  } 
  
}