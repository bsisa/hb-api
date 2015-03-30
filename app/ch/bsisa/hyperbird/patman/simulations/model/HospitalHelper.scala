package ch.bsisa.hyperbird.patman.simulations.model

import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.util.DateUtil

/**
 *  Helper to go from ELFIN to Hospital and reverse.
 *
 * <L POS="5">
 *   <C POS="1">503B</C>
 *   <C POS="2">4195904</C>
 *   <C POS="3">soins continus</C>
 *   <C POS="4">médicalisé</C>
 *   <C POS="5">terminé</C>
 *   <C POS="6">occupé</C>
 *   <C POS="7"/>
 * </L>
 *
 */
object HospitalHelper {

  def toHospital(elfinHospitalState: ELFIN): Hospital = {
    val hospitalCode = getMixedContent(elfinHospitalState.CARACTERISTIQUE.get.FRACTION.get.L(0).C(0).mixed)
    val scheduleStr = elfinHospitalState.IDENTIFIANT.get.DE.get
    //play.api.Logger.info("scheduleStr = " + scheduleStr)
    val schedule = DateUtil.getIsoDateFormatterWithoutTz.parse(scheduleStr)
    val beds = for {
      (l, index) <- (elfinHospitalState.CARACTERISTIQUE.get.FRACTION.get.L zipWithIndex) if (index > 0) // First L does not contain bed information
    } yield {
      val c = l.C
      val bedId = getMixedContent(c(0).mixed)
      val bedFree = (getMixedContent(c(5).mixed) == BED_FREE_CODE)
      val patientNb = getMixedContent(c(1).mixed)
      val patientType = getMixedContent(c(2).mixed)
      val transferType = getMixedContent(c(3).mixed)
      val bed = Bed(bedId, bedFree, patientNb, patientType, transferType)
      bed
    }

    Hospital(hospitalCode, schedule, beds)
  }

}