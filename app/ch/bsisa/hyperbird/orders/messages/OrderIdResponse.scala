package ch.bsisa.hyperbird.orders.messages

/**
 * A response to request for new orders dedicated id number.
 *
 * The id value contained into this response is optional meaning 
 * that if the orders id service is running correctly Some value
 * is returned otherwise if None is returned the service is not 
 * available, for instance not yet initialised.
 * 
 * @author Patrick Refondini
 */
case class OrderIdResponse(id : Option[Int]) {

}