package ch.bsisa.hyperbird.patman.simulations

object Constants {

  val HOSPITAL_CODE_CDF = "cdf"
  val HOSPITAL_CODE_PRT = "prt"

  val BED_FREE_CODE = "libre"
  val BED_BUSY_CODE = "occupé"

  val PATIENT_TYPE_SI = "soins intensifs"
  val PATIENT_TYPE_SC = "soins continus"

  val TRANSFER_TYPE_SPEC = "spécialisé"
  val TRANSFER_TYPE_MED = "médicalisé"
  val TRANSFER_TYPE_NON_MED = "non-médicalisé"
    
  val TRANSFER_REQUEST_ACCEPTED = "accepted" 
  val TRANSFER_REQUEST_REFUSED = "refused"
  val TRANSFER_REQUEST_PARTIAL = "partial"
    
  val DATASET_UPDATE_REQUEST_SUCCESS = "success" 
  val DATASET_UPDATE_REQUEST_FAILURE = "failure"
    
}