package ch.bsisa.hyperbird.patman.simulations.messages

import java.util.Date

case class DataSetUpdateResponse(status: String, fromHospitalCode:String, toHospitalCode:String, fromSchedule:Date) {

}