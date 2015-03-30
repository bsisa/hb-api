package ch.bsisa.hyperbird.patman.simulations.messages

import ch.bsisa.hyperbird.patman.simulations.model.Bed

/**
 * `id` correlation identifier.
 * `status` {accepted, refused, partial} see simulations.Constants
 */
case class TransferResponse(id: String, status: String, acceptedIncomingBeds: List[Bed], fromHospitalCode: String, toHospitalCode: String, messagemessage: String) {

}