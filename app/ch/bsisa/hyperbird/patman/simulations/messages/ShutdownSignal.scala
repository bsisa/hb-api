package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.HospitalSimulationSummary

case class ShutdownSignal(message:String, terminationSize:Int, simulationSummary: Option[HospitalSimulationSummary] = None) {

}