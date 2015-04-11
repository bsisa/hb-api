package ch.bsisa.hyperbird.patman.simulations.messages


case class TransferResponseUpdate(correlationId: String, status:Boolean, fromHospitalCode: String, toHospitalCode: String, message: String) {

}