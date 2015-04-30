package test.ch.bsisa.hyperbird.patman.simulations.model

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import org.specs2.mutable._
import scala.xml.XML
import play.api.libs.json.Json
import play.api.libs.json._
import ch.bsisa.hyperbird.patman.simulations.Constants
import ch.bsisa.hyperbird.patman.simulations.model.Bed
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.util.DateUtil
import ch.bsisa.hyperbird.patman.simulations.model.HospitalSimulationSummary
import java.util.Date
import ch.bsisa.hyperbird.model.format.ElfinFormat

/**
 * Tests HospitalHelperSimpleSpec
 *
 * 
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.patman.simulations.model.HospitalHelperSimpleSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class HospitalHelperSimpleSpec extends Specification {

  val currentDate = new Date()

  val hospitalWithEmptyBeds: Hospital = Hospital("cdf", currentDate, List())

  val hospitalWithEmptyBedsElfin: ELFIN = HospitalHelper.toElfin(hospitalWithEmptyBeds)

  "The hospitalWithEmptyBeds" should {
    s"have beds List equal to nil" in {
      hospitalWithEmptyBeds.beds mustEqual List()
    }
  }

  val expectedDateStr = DateUtil.getIsoDateFormatterWithoutTz.format(currentDate)

  "The hospitalWithEmptyBedsElfin" should {
    s"must have IDENTIFIANT.ALIAS equal to 'cdf'" in {
      hospitalWithEmptyBedsElfin.IDENTIFIANT.get.ALIAS.get mustEqual "cdf"
    }
    s"must have IDENTIFIANT. equal to '${expectedDateStr}'" in {
      hospitalWithEmptyBedsElfin.IDENTIFIANT.get.DE.get mustEqual expectedDateStr
    }
    s"must contain Some(CARACTERISTIQUE/FRACTION) element" in {
      hospitalWithEmptyBedsElfin.CARACTERISTIQUE.get.FRACTION must beSome[ch.bsisa.hyperbird.model.MATRICETypable]
    }
    s"must contain an empty list L (CARACTERISTIQUE/FRACTION/L) element" in {
      hospitalWithEmptyBedsElfin.CARACTERISTIQUE.get.FRACTION.get.L mustEqual List()
    }
  }
//  val hsXml = ElfinFormat.toXml(hospitalWithEmptyBedsElfin)
//  println(hsXml)

}
