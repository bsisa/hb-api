package ch.bsisa.hyperbird.patman.simulations.model

/**
 * Models a bed and its associated patient information
 */
case class Bed(id: String, free: Boolean, patientNb: String, patientType: String, transferType: String, reasonForTransfer: Option[String] = None) {

  override def hashCode = patientNb.hashCode()
  override def equals(other: Any): Boolean = other match {
    case that: Bed => (
      that.canEqual(this)
      && this.patientNb == that.patientNb)
    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[Bed]
}
