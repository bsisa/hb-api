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

case object GetDestination extends ActviewMessage
case class SetDestination(position:POINT) extends ActviewMessage
case class Destination(objectId:String, position : Option[POINT]) extends ActviewMessage

case object GetPosition extends ActviewMessage
case class Position(objectId:String, position:POINT) extends ActviewMessage
