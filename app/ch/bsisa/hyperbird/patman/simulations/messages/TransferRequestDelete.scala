package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed
import java.util.Date

case class TransferRequestDelete(id: String, bedsWithOutgoingPatientTypeSi: List[Bed], bedsWithOutgoingPatientTypeSc: List[Bed], fromHospitalCode: String, toHospitalCode: String, fromSchedule: Date, message: String) {

}