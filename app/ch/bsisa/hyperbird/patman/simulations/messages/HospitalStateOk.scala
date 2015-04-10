package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.model.ELFIN

case class HospitalStateOk(elfin:ELFIN, fromHospital:String, previousSimulatedHospitalState : Option[Hospital]) {

}