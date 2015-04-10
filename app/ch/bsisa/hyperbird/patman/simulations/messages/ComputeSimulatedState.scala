package ch.bsisa.hyperbird.patman.simulations.messages

import akka.actor.ActorRef
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.model.ELFIN

case class ComputeSimulatedState(elfin:ELFIN, transferActorRef: ActorRef, previousSimulatedHospitalState: Option[Hospital]) {

}