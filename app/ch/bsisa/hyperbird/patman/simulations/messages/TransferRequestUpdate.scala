package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed
import java.util.Date


// TODO: a single List[Bed] parameter is required: updatedBeds: List[Bed] all other cases are handled by Create/Delete requests.  
case class TransferRequestUpdate(id: String, incomingSiBeds: List[Bed], outgoingSiBeds: List[Bed], typeScToSiBeds: List[Bed], fromHospitalCode: String, toHospitalCode: String, fromSchedule: Date, message: String) {

}