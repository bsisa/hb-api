package ch.bsisa.hyperbird.orders.messages

/**
 * Message used to deliver current orders max value used 
 * primarily to initialise the orders Id service ( `OrdersIdActor` )
 * from database max value at system start up.
 * 
 * @author Patrick Refondini
 */
case class OrdersMaxValueInit(maxValue : Int) {

}