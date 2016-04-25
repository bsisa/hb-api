/**
 *
 */
package ch.bsisa.hyperbird.actview

import ch.bsisa.hyperbird.model.POINT

/**
 * @author Patrick Refondini
 *
 */
sealed trait ActviewMessage 

case class LoadFleet(objCollection:String, objClass:String) extends ActviewMessage
case object DeleteFleet extends ActviewMessage
case class BroadcastFleet(message:String) extends ActviewMessage

case class GetDestination(fleetName:String, objectId:String) extends ActviewMessage
case class SetDestination(fleetName:String, objectId:String, position:POINT) extends ActviewMessage
case class Destination(fleetName:String, objectId:String, position:POINT) extends ActviewMessage

case class GetPosition(fleetName:String, objectId:String) extends ActviewMessage
case class Position(fleetName:String, objectId:String, position:POINT) extends ActviewMessage
