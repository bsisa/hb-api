package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed
import java.util.Date

// TODO: remove outgoingSiBeds. This is only meaningful to TransferRequestDelete
case class TransferRequestCreate(id: String, incomingSiBeds: List[Bed], outgoingSiBeds: List[Bed], typeScToSiBeds: List[Bed], fromHospitalCode: String, toHospitalCode: String, fromSchedule: Date, message: String) {

}