package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed

case class TransferRequest(id: String, incomingBeds: List[Bed], outgoingBeds: List[Bed], fromHospitalCode: String, toHospitalCode: String, message: String) {

}