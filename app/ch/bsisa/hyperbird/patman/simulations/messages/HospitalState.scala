package ch.bsisa.hyperbird.patman.simulations.messages;

import ch.bsisa.hyperbird.model.ELFIN
import akka.actor.ActorRef

case class HospitalState(elfin : ELFIN, transferActorRef: ActorRef) {

}
