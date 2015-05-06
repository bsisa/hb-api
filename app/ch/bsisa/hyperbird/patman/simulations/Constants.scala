package ch.bsisa.hyperbird.patman.simulations

/**
 * Patman Simulation related constant values.
 * 
 * @author Patrick Refondini
 */
object Constants {

  val HOSPITAL_CODE_CDF = "cdf"
  val HOSPITAL_CODE_PRT = "prt"

  val BED_FREE_CODE = "libre"
  val BED_BUSY_CODE = "occupé"
    
  val BED_PENDING_INPUT = "en cours"
  val BED_COMPLETED_INPUT = "terminé"

  val BED_REASON_FOR_TRANSFER_NONE = "none"
  val BED_REASON_FOR_TRANSFER_SI = "SI"
  val BED_REASON_FOR_TRANSFER_SI_TO_SC = "SI to SC"
  val BED_REASON_FOR_TRANSFER_SC = "SC"
  val BED_REASON_FOR_TRANSFER_SC_TO_SI = "SC to SI"
  val BED_REASON_FOR_TRANSFER_TRANSFER_TYPE_CHANGE_FOR_SI = "transfer type change for SI"
  val BED_REASON_FOR_TRANSFER_TRANSFER_TYPE_CHANGE_FOR_SC = "transfer type change for SC"
    
  val PATIENT_TYPE_SI = "soins intensifs"
  val PATIENT_TYPE_SC = "soins continus"

  val TRANSFER_TYPE_SPEC = "spécialisé"
  val TRANSFER_TYPE_MED = "médicalisé"
  val TRANSFER_TYPE_NON_MED = "non-médicalisé"

  val TRANSFER_REQUEST_ACCEPTED = "accepted"
  val TRANSFER_REQUEST_REFUSED = "refused"
  val TRANSFER_REQUEST_PARTIAL = "partial"

  val DATASET_UPDATE_RESPONSE_SUCCESS = "success"
  val DATASET_UPDATE_RESPONSE_FAILURE = "failure"

  val TRANSFER_NATURE_ADD = "add"
  val TRANSFER_NATURE_REMOVE = "remove"
  val TRANSFER_NATURE_UPDATE = "update"

  val ELFIN_HOSPITAL_STATE_SIMULATION_NATURE = "simulation"

  val ELFIN_TRANSFER_SIMULATION_COLLECTION_ID = "G20150114160000005"
  val ELFIN_HOSPITAL_STATE_SIMULATION_COLLECTION_ID = "G20150114160000006"
  val ELFIN_SIMULATION_COLLECTION_ID = "G20150114160000007"

}