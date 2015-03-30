package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed

case class TransferRequest(id: String, incomingSiBeds: List[Bed], outgoingSiBeds: List[Bed], typeScToSiBeds: List[Bed], fromHospitalCode: String, toHospitalCode: String, message: String) {

}