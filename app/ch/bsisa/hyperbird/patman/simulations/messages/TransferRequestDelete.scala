package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed
import java.util.Date

// TODO: remove incomingSiBeds and typeScToSiBeds parameters, these are only meaningful to TransferRequestCreate
case class TransferRequestDelete(id: String, incomingSiBeds: List[Bed], outgoingSiBeds: List[Bed], typeScToSiBeds: List[Bed], fromHospitalCode: String, toHospitalCode: String, fromSchedule: Date, message: String) {

}