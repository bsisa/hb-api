package ch.bsisa.hyperbird.patman.simulations.messages

import java.util.Date
import ch.bsisa.hyperbird.patman.simulations.model.Bed

case class DataSetUpdateResponse(id : String, status: String, allTransferredSiBeds : List[Bed], fromHospitalCode:String, toHospitalCode:String, fromSchedule:Date) {

}