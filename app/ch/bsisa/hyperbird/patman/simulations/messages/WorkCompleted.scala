package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.HospitalSimulationSummary

case class WorkCompleted(message:String, simulationSummary: Option[HospitalSimulationSummary] = None) {

}