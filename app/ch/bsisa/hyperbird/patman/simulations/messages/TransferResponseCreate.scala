package ch.bsisa.hyperbird.patman.simulations.messages


case class TransferResponseCreate(correlationId: String, status:Boolean, fromHospitalCode: String, toHospitalCode: String, message: String) {

}