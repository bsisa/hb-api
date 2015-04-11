package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed
import java.util.Date

case class TransferRequestCreate(id: String, bedsWithIncomingPatientTypeSi: List[Bed], patientTypeChangeFromScToSi: List[Bed], fromHospitalCode: String, toHospitalCode: String, fromSchedule: Date, message: String) {

}