package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed
import java.util.Date

case class TransferRequest(id: String, incomingSiBeds: List[Bed], outgoingSiBeds: List[Bed], typeScToSiBeds: List[Bed], fromHospitalCode: String, toHospitalCode: String, fromSchedule: Date, message: String) {

}