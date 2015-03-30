package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed
import java.util.Date


case class DataSetUpdateRequest(id : String, transferredBeds: List[Bed], fromHospital:String, toHospital:String, fromSchedule:Date) {

}