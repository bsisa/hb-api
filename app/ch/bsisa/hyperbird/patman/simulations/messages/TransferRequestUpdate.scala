package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed
import java.util.Date

case class TransferRequestUpdate(id: String, patientTypeChangeFromSiToSc: List[Bed], bedsWithTransferTypeOnlyChangePatientTypeSi: List[Bed], bedsWithTransferTypeOnlyChangePatientTypeSc: List[Bed], fromHospitalCode: String, toHospitalCode: String, fromSchedule: Date, message: String) {

}