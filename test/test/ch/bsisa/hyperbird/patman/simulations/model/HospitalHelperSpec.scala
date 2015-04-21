package test.ch.bsisa.hyperbird.util

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import org.specs2.mutable._
import scala.xml.XML
import play.api.libs.json.Json
import play.api.libs.json._
import ch.bsisa.hyperbird.patman.simulations.model.Bed
import ch.bsisa.hyperbird.patman.simulations.model.HospitalHelper
import ch.bsisa.hyperbird.patman.simulations.model.Hospital
import ch.bsisa.hyperbird.util.DateUtil
import ch.bsisa.hyperbird.patman.simulations.model.HospitalSimulationSummary

/**
 * Tests HospitalHelper
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.util.HospitalHelperSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class HospitalHelperSpec extends BaseSerialisationSpec {

  val EXPECTED_CDF_CODE = "cdf"
  val EXPECTED_SCHEDULE_1_STR = "2015-01-14T08:00:00+01:00"
  val EXPECTED_SCHEDULE_2_STR = "2015-01-14T16:00:00+01:00"
  val EXPECTED_SCHEDULE_3_STR = "2015-01-14T22:00:00+01:00"
  val EXPECTED_SCHEDULE_4_STR = "2015-01-15T08:00:00+01:00"
  val EXPECTED_SCHEDULE_5_STR = "2015-01-15T16:00:00+01:00"
  val EXPECTED_SCHEDULE_6_STR = "2015-01-15T22:00:00+01:00"
  val EXPECTED_SCHEDULE_7_STR = "2015-01-16T08:00:00+01:00"
  val EXPECTED_SCHEDULE_8_STR = "2015-01-16T16:00:00+01:00"
  val EXPECTED_SCHEDULE_9_STR = "2015-01-16T22:00:00+01:00"

  val simulationsTestResourcesPath = TestResourcesDir + "simulations/"
  val hospitalState1ElfinFile = "HOSPITAL_STATE_1.xml"
  val hospitalState2ElfinFile = "HOSPITAL_STATE_2.xml"
  val hospitalState3ElfinFile = "HOSPITAL_STATE_3.xml"
  val hospitalState4ElfinFile = "HOSPITAL_STATE_4.xml"
  val hospitalState5ElfinFile = "HOSPITAL_STATE_5.xml"
  val hospitalState6ElfinFile = "HOSPITAL_STATE_6.xml"
  val hospitalState7ElfinFile = "HOSPITAL_STATE_7.xml"
  val hospitalState8ElfinFile = "HOSPITAL_STATE_8.xml"
  val hospitalState9ElfinFile = "HOSPITAL_STATE_9.xml"

  // ==================================================================
  // XML file => scala.xml.Elem
  // ==================================================================    
  val hospitalState1Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState1ElfinFile)
  val hospitalState2Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState2ElfinFile)
  val hospitalState3Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState3ElfinFile)
  val hospitalState4Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState4ElfinFile)
  val hospitalState5Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState5ElfinFile)
  val hospitalState6Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState6ElfinFile)
  val hospitalState7Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState7ElfinFile)
  val hospitalState8Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState8ElfinFile)
  val hospitalState9Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState9ElfinFile)

  // ==================================================================
  // scala.xml.Elem => ch.bsisa.hyperbird.model.ELFIN
  // ==================================================================
  val elfinHs1 = scalaxb.fromXML[ELFIN](hospitalState1Xml)
  val elfinHs2 = scalaxb.fromXML[ELFIN](hospitalState2Xml)
  val elfinHs3 = scalaxb.fromXML[ELFIN](hospitalState3Xml)
  val elfinHs4 = scalaxb.fromXML[ELFIN](hospitalState4Xml)
  val elfinHs5 = scalaxb.fromXML[ELFIN](hospitalState5Xml)
  val elfinHs6 = scalaxb.fromXML[ELFIN](hospitalState6Xml)
  val elfinHs7 = scalaxb.fromXML[ELFIN](hospitalState7Xml)
  val elfinHs8 = scalaxb.fromXML[ELFIN](hospitalState8Xml)
  val elfinHs9 = scalaxb.fromXML[ELFIN](hospitalState9Xml)

  // Convert generic ELFIN data structure of CLASSE='HOSPITAL_STATE' to semantic type Hospital 
  val hospitalState1 = HospitalHelper.toHospital(elfinHs1)
  val hospitalState2 = HospitalHelper.toHospital(elfinHs2)
  val hospitalState3 = HospitalHelper.toHospital(elfinHs3)
  val hospitalState4 = HospitalHelper.toHospital(elfinHs4)
  val hospitalState5 = HospitalHelper.toHospital(elfinHs5)
  val hospitalState6 = HospitalHelper.toHospital(elfinHs6)
  val hospitalState7 = HospitalHelper.toHospital(elfinHs7)
  val hospitalState8 = HospitalHelper.toHospital(elfinHs8)
  val hospitalState9 = HospitalHelper.toHospital(elfinHs9)

  // ================================================================== 
  // 	WARNING - Tests MUST be sequential to succeed  
  // ==================================================================  
  // Mutable references to previous/current hospital states
  var previousHospitalState: Option[Hospital] = None
  var currentHospitalState: Option[Hospital] = None

  /**
   * Dynamic state representation build from HOSPITAL_STATE database entries
   * change events from schedule to schedule (08:00, 16:00, 22:00)
   */
  var simulatedHospitalState: Option[Hospital] = None

  /**
   * Maintained hospital aggregated figures delivered at simulation end.
   */
  var simulationSummary: Option[HospitalSimulationSummary] = None

  // ==================================================================

  // ==================================================================
  // 	Check loaded / transformed test data are what we expect 
  // ==================================================================

  "The hospitalState1" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState1.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_1_STR}'" in {
      hospitalState1.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_1_STR)
    }
    s"have 10 beds" in {
      hospitalState1.beds must have size (10)
    }
    s"have 3 busy beds" in {
      hospitalState1.beds.filterNot(b => b.free) must have size (3)
    }
    s"have 7 free beds" in {
      hospitalState1.beds.filter(b => b.free) must have size (7)
    }
  }

  "The hospitalState2" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState2.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_2_STR}'" in {
      hospitalState2.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_2_STR)
    }
  }

  "The hospitalState3" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState3.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_3_STR}'" in {
      hospitalState3.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_3_STR)
    }
  }

  "The hospitalState4" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState4.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_4_STR}'" in {
      hospitalState4.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_4_STR)
    }
  }

  "The hospitalState5" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState5.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_5_STR}'" in {
      hospitalState5.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_5_STR)
    }
  }

  "The hospitalState6" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState6.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_6_STR}'" in {
      hospitalState6.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_6_STR)
    }
  }

  "The hospitalState7" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState7.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_7_STR}'" in {
      hospitalState7.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_7_STR)
    }
  }

  "The hospitalState8" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState8.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_8_STR}'" in {
      hospitalState8.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_8_STR)
    }
  }

  "The hospitalState9" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState9.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_9_STR}'" in {
      hospitalState9.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_9_STR)
    }
  }

  // ==================================================================
  // 	Test dynamic simulation behaviour  
  // ==================================================================  

  rollHospitalStates(hospitalState1)

  HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    case (
      bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
      bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
      patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
      bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

      // Update hospital simulation summary
      simulationSummary = Some(
        HospitalHelper.updateHospitalSimulationSummary(
          hospitalCode = EXPECTED_CDF_CODE,
          currentHss = simulationSummary,
          bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi,
          bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc,
          bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi,
          bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc))

      // ================================================================================================================================= 
      // Update current CDT simulatedHospitalState removing transfered SI beds
      // =================================================================================================================================
      // TODO: Full implementation review needed. Follow above algorithm description (UPDATE ALGO CDF)
      simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForCdf(
        currentSimulatedHospitalStateOption = simulatedHospitalState,
        newStaticHospitalStateOption = currentHospitalState,
        bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
        bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
        patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
        bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc)
  }

  /**
   * Mutates var references.
   */
  def rollHospitalStates(newHospitalState: Hospital) = {
    previousHospitalState = currentHospitalState
    currentHospitalState = Some(newHospitalState)
  }

}
