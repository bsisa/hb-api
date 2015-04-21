package test.ch.bsisa.hyperbird.util

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

      testBedsUpdatesOutcome(hospitalStateNb = 1,
        bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, expectedBedsWithIncomingPatientTypeSiNb = 2,
        bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, expectedBedsWithIncomingPatientTypeScNb = 1,
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, expectedBedsWithOutgoingPatientTypeSiNb = 0,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc, expectedBedsWithOutgoingPatientTypeScNb = 0,
        patientTypeChangeFromScToSi = patientTypeChangeFromScToSi, expectedPatientTypeChangeFromScToSiNb = 0,
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, expectedPatientTypeChangeFromSiToScNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi, expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChangePatientTypeSc, expectedBedsWithTransferTypeOnlyChangePatientTypeScNb = 0)

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

  rollHospitalStates(hospitalState2)

  HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    case (
      bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
      bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
      patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
      bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

      testBedsUpdatesOutcome(hospitalStateNb = 2,
        bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, expectedBedsWithIncomingPatientTypeSiNb = 1,
        bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, expectedBedsWithIncomingPatientTypeScNb = 1,
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, expectedBedsWithOutgoingPatientTypeSiNb = 0,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc, expectedBedsWithOutgoingPatientTypeScNb = 0,
        patientTypeChangeFromScToSi = patientTypeChangeFromScToSi, expectedPatientTypeChangeFromScToSiNb = 0,
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, expectedPatientTypeChangeFromSiToScNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi, expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChangePatientTypeSc, expectedBedsWithTransferTypeOnlyChangePatientTypeScNb = 0)

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

  rollHospitalStates(hospitalState3)

  HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    case (
      bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
      bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
      patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
      bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

      testBedsUpdatesOutcome(hospitalStateNb = 3,
        bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, expectedBedsWithIncomingPatientTypeSiNb = 0,
        bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, expectedBedsWithIncomingPatientTypeScNb = 1,
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, expectedBedsWithOutgoingPatientTypeSiNb = 0,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc, expectedBedsWithOutgoingPatientTypeScNb = 1,
        patientTypeChangeFromScToSi = patientTypeChangeFromScToSi, expectedPatientTypeChangeFromScToSiNb = 0,
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, expectedPatientTypeChangeFromSiToScNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi, expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChangePatientTypeSc, expectedBedsWithTransferTypeOnlyChangePatientTypeScNb = 0)

      val incomingScBed = bedsWithIncomingPatientTypeSc(0)
      val outgoingScBed = bedsWithOutgoingPatientTypeSc(0)

      s"Incoming SC bed" should {
        s"have patient id 9993784" in {
          incomingScBed.patientNb mustEqual "9993784"
        }
        s"have patient type ${Constants.PATIENT_TYPE_SC}" in {
          incomingScBed.patientType mustEqual Constants.PATIENT_TYPE_SC
        }
        s"have transfer type ${Constants.TRANSFER_TYPE_MED}" in {
          incomingScBed.transferType mustEqual Constants.TRANSFER_TYPE_MED
        }
      }

      s"Outgoing SC bed" should {
        s"have patient id 9990360" in {
          outgoingScBed.patientNb mustEqual "9990360"
        }
        s"have patient type ${Constants.PATIENT_TYPE_SC}" in {
          outgoingScBed.patientType mustEqual Constants.PATIENT_TYPE_SC
        }
        s"have transfer type ${Constants.TRANSFER_TYPE_MED}" in {
          outgoingScBed.transferType mustEqual Constants.TRANSFER_TYPE_MED
        }
      }

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

  rollHospitalStates(hospitalState4)

  HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    case (
      bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
      bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
      patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
      bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

      testBedsUpdatesOutcome(hospitalStateNb = 4,
        bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, expectedBedsWithIncomingPatientTypeSiNb = 1,
        bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, expectedBedsWithIncomingPatientTypeScNb = 0,
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, expectedBedsWithOutgoingPatientTypeSiNb = 0,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc, expectedBedsWithOutgoingPatientTypeScNb = 0,
        patientTypeChangeFromScToSi = patientTypeChangeFromScToSi, expectedPatientTypeChangeFromScToSiNb = 0,
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, expectedPatientTypeChangeFromSiToScNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi, expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChangePatientTypeSc, expectedBedsWithTransferTypeOnlyChangePatientTypeScNb = 0)

      val incomingSiBed = bedsWithIncomingPatientTypeSi(0)

      s"Incoming SI bed" should {
        s"have patient id 9997101" in {
          incomingSiBed.patientNb mustEqual "9997101"
        }
        s"have patient type ${Constants.PATIENT_TYPE_SI}" in {
          incomingSiBed.patientType mustEqual Constants.PATIENT_TYPE_SI
        }
        s"have transfer type ${Constants.TRANSFER_TYPE_MED}" in {
          incomingSiBed.transferType mustEqual Constants.TRANSFER_TYPE_MED
        }
        s"have bed id 504B" in {
          incomingSiBed.id mustEqual "504B"
        }
      }

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
   * STEP 5
   *
   * 501
   * 9997922 SC out MED
   * 9997419 SI in SPEC
   *
   * 504B => 502
   * 9997101 SI MED - no change
   *
   * 503A
   * 9995826 SI to SC    pat type change
   *         SPEC to MED trn type change
   *
   * 506
   * 9993784 SC out MED
   * 9990075 SI in  SPEC
   *
   * SI in    size 2
   * SC in    size 0
   * SI out   size 0
   * SC out   size 2
   * SI to SC size 1
   *
   */
  rollHospitalStates(hospitalState5)

  HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    case (
      bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
      bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
      patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
      bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

      testBedsUpdatesOutcome(hospitalStateNb = 5,
        bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, expectedBedsWithIncomingPatientTypeSiNb = 2,
        bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, expectedBedsWithIncomingPatientTypeScNb = 0,
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, expectedBedsWithOutgoingPatientTypeSiNb = 0,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc, expectedBedsWithOutgoingPatientTypeScNb = 2,
        patientTypeChangeFromScToSi = patientTypeChangeFromScToSi, expectedPatientTypeChangeFromScToSiNb = 0,
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, expectedPatientTypeChangeFromSiToScNb = 1,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi, expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChangePatientTypeSc, expectedBedsWithTransferTypeOnlyChangePatientTypeScNb = 0)

      //      val incomingSiBed = bedsWithIncomingPatientTypeSi(0)
      //      
      //      s"Incoming SI bed" should {
      //        s"have patient id 9997101" in {
      //          incomingSiBed.patientNb mustEqual "9997101"
      //        }
      //        s"have patient type ${Constants.PATIENT_TYPE_SI}" in {
      //          incomingSiBed.patientType mustEqual Constants.PATIENT_TYPE_SI
      //        }
      //        s"have transfer type ${Constants.TRANSFER_TYPE_MED}" in {
      //          incomingSiBed.transferType mustEqual Constants.TRANSFER_TYPE_MED
      //        }
      //        s"have bed id 504B" in {
      //        	incomingSiBed.id mustEqual "504B"  
      //        }
      //      }

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
   * STEP 6
   * 5 SI, 1 SC, SI transfer type change only.
   *
   */
  rollHospitalStates(hospitalState6)

  HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    case (
      bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
      bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
      patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
      bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

      testBedsUpdatesOutcome(hospitalStateNb = 6,
        bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, expectedBedsWithIncomingPatientTypeSiNb = 0,
        bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, expectedBedsWithIncomingPatientTypeScNb = 0,
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, expectedBedsWithOutgoingPatientTypeSiNb = 0,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc, expectedBedsWithOutgoingPatientTypeScNb = 0,
        patientTypeChangeFromScToSi = patientTypeChangeFromScToSi, expectedPatientTypeChangeFromScToSiNb = 0,
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, expectedPatientTypeChangeFromSiToScNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi, expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb = 1,
        bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChangePatientTypeSc, expectedBedsWithTransferTypeOnlyChangePatientTypeScNb = 0)

      val bedWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi(0)

      s"SI bed with transfer type change" should {
        s"have patient id 9997419" in {
          bedWithTransferTypeOnlyChangePatientTypeSi.patientNb mustEqual "9997419"
        }
        s"have patient type ${Constants.PATIENT_TYPE_SI}" in {
          bedWithTransferTypeOnlyChangePatientTypeSi.patientType mustEqual Constants.PATIENT_TYPE_SI
        }
        s"have transfer type ${Constants.TRANSFER_TYPE_MED}" in {
          bedWithTransferTypeOnlyChangePatientTypeSi.transferType mustEqual Constants.TRANSFER_TYPE_MED
        }
        s"have bed id 501" in {
          bedWithTransferTypeOnlyChangePatientTypeSi.id mustEqual "501"
        }
      }

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
   * STEP 7
   * 505
   * 9991663 SI to SC 
   *
   */
  rollHospitalStates(hospitalState7)

  HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    case (
      bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
      bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
      patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
      bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

      testBedsUpdatesOutcome(hospitalStateNb = 7,
        bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, expectedBedsWithIncomingPatientTypeSiNb = 0,
        bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, expectedBedsWithIncomingPatientTypeScNb = 0,
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, expectedBedsWithOutgoingPatientTypeSiNb = 0,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc, expectedBedsWithOutgoingPatientTypeScNb = 0,
        patientTypeChangeFromScToSi = patientTypeChangeFromScToSi, expectedPatientTypeChangeFromScToSiNb = 0,
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, expectedPatientTypeChangeFromSiToScNb = 1,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi, expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChangePatientTypeSc, expectedBedsWithTransferTypeOnlyChangePatientTypeScNb = 0)

      val bedWithpatientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc(0)

      s"SI bed with transfer type change" should {
        s"have patient id 9991663" in {
          bedWithpatientTypeChangeFromSiToSc.patientNb mustEqual "9991663"
        }
        s"have patient type ${Constants.PATIENT_TYPE_SC}" in {
          bedWithpatientTypeChangeFromSiToSc.patientType mustEqual Constants.PATIENT_TYPE_SC
        }
        s"have transfer type ${Constants.TRANSFER_TYPE_MED}" in {
          bedWithpatientTypeChangeFromSiToSc.transferType mustEqual Constants.TRANSFER_TYPE_MED
        }
        s"have bed id 505" in {
          bedWithpatientTypeChangeFromSiToSc.id mustEqual "505"
        }
      }

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
   * STEP 8
   * 501
   * 9997419 SI out
   *
   */
  rollHospitalStates(hospitalState8)

  HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    case (
      bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
      bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
      patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
      bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

      testBedsUpdatesOutcome(hospitalStateNb = 8,
        bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, expectedBedsWithIncomingPatientTypeSiNb = 0,
        bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, expectedBedsWithIncomingPatientTypeScNb = 0,
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, expectedBedsWithOutgoingPatientTypeSiNb = 1,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc, expectedBedsWithOutgoingPatientTypeScNb = 0,
        patientTypeChangeFromScToSi = patientTypeChangeFromScToSi, expectedPatientTypeChangeFromScToSiNb = 0,
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, expectedPatientTypeChangeFromSiToScNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi, expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChangePatientTypeSc, expectedBedsWithTransferTypeOnlyChangePatientTypeScNb = 0)

      val bedWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi(0)

      s"SI bed with transfer type change" should {
        s"have patient id 9997419" in {
          bedWithOutgoingPatientTypeSi.patientNb mustEqual "9997419"
        }
        s"have patient type ${Constants.PATIENT_TYPE_SI}" in {
          bedWithOutgoingPatientTypeSi.patientType mustEqual Constants.PATIENT_TYPE_SI
        }
        s"have transfer type ${Constants.TRANSFER_TYPE_MED}" in {
          bedWithOutgoingPatientTypeSi.transferType mustEqual Constants.TRANSFER_TYPE_MED
        }
        s"have bed id 501" in {
          bedWithOutgoingPatientTypeSi.id mustEqual "501"
        }
      }

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
   * STEP 9
   * 503A
   * 9995826 SC out MED
   *
   * 504B
   * 9997437 SC in MED
   */
  rollHospitalStates(hospitalState9)

  HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
    case (
      bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
      bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
      patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
      bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

      testBedsUpdatesOutcome(hospitalStateNb = 9,
        bedsWithIncomingPatientTypeSi = bedsWithIncomingPatientTypeSi, expectedBedsWithIncomingPatientTypeSiNb = 0,
        bedsWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc, expectedBedsWithIncomingPatientTypeScNb = 1,
        bedsWithOutgoingPatientTypeSi = bedsWithOutgoingPatientTypeSi, expectedBedsWithOutgoingPatientTypeSiNb = 0,
        bedsWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc, expectedBedsWithOutgoingPatientTypeScNb = 1,
        patientTypeChangeFromScToSi = patientTypeChangeFromScToSi, expectedPatientTypeChangeFromScToSiNb = 0,
        patientTypeChangeFromSiToSc = patientTypeChangeFromSiToSc, expectedPatientTypeChangeFromSiToScNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSi = bedsWithTransferTypeOnlyChangePatientTypeSi, expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb = 0,
        bedsWithTransferTypeOnlyChangePatientTypeSc = bedsWithTransferTypeOnlyChangePatientTypeSc, expectedBedsWithTransferTypeOnlyChangePatientTypeScNb = 0)

      val bedWithIncomingPatientTypeSc = bedsWithIncomingPatientTypeSc(0)
      val bedWithOutgoingPatientTypeSc = bedsWithOutgoingPatientTypeSc(0)

      s"incoming SC bed" should {
        s"have patient id 9997437" in {
          bedWithIncomingPatientTypeSc.patientNb mustEqual "9997437"
        }
        s"have patient type ${Constants.PATIENT_TYPE_SC}" in {
          bedWithIncomingPatientTypeSc.patientType mustEqual Constants.PATIENT_TYPE_SC
        }
        s"have transfer type ${Constants.TRANSFER_TYPE_MED}" in {
          bedWithIncomingPatientTypeSc.transferType mustEqual Constants.TRANSFER_TYPE_MED
        }
        s"have bed id 504B" in {
          bedWithIncomingPatientTypeSc.id mustEqual "504B"
        }
      }      

      s"outgoing SC bed" should {
        s"have patient id 9995826" in {
          bedWithOutgoingPatientTypeSc.patientNb mustEqual "9995826"
        }
        s"have patient type ${Constants.PATIENT_TYPE_SC}" in {
          bedWithOutgoingPatientTypeSc.patientType mustEqual Constants.PATIENT_TYPE_SC
        }
        s"have transfer type ${Constants.TRANSFER_TYPE_MED}" in {
          bedWithOutgoingPatientTypeSc.transferType mustEqual Constants.TRANSFER_TYPE_MED
        }
        s"have bed id 503A" in {
          bedWithOutgoingPatientTypeSc.id mustEqual "503A"
        }
      }      
      
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

  def testBedsUpdatesOutcome(hospitalStateNb: Int,
    bedsWithIncomingPatientTypeSi: List[Bed], expectedBedsWithIncomingPatientTypeSiNb: Int,
    bedsWithIncomingPatientTypeSc: List[Bed], expectedBedsWithIncomingPatientTypeScNb: Int,
    bedsWithOutgoingPatientTypeSi: List[Bed], expectedBedsWithOutgoingPatientTypeSiNb: Int,
    bedsWithOutgoingPatientTypeSc: List[Bed], expectedBedsWithOutgoingPatientTypeScNb: Int,
    patientTypeChangeFromScToSi: List[Bed], expectedPatientTypeChangeFromScToSiNb: Int,
    patientTypeChangeFromSiToSc: List[Bed], expectedPatientTypeChangeFromSiToScNb: Int,
    bedsWithTransferTypeOnlyChangePatientTypeSi: List[Bed], expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb: Int,
    bedsWithTransferTypeOnlyChangePatientTypeSc: List[Bed], expectedBedsWithTransferTypeOnlyChangePatientTypeScNb: Int): Unit = {

    s"Processing hospitalState${hospitalStateNb}" should {
      s"provide ${expectedBedsWithIncomingPatientTypeSiNb} incoming SI beds" in {
        bedsWithIncomingPatientTypeSi must have size expectedBedsWithIncomingPatientTypeSiNb
      }
      s"provide ${expectedBedsWithIncomingPatientTypeScNb} incoming SC bed" in {
        bedsWithIncomingPatientTypeSc must have size expectedBedsWithIncomingPatientTypeScNb
      }
      s"provide ${expectedBedsWithOutgoingPatientTypeSiNb} outgoing SI bed" in {
        bedsWithOutgoingPatientTypeSi must have size expectedBedsWithOutgoingPatientTypeSiNb
      }
      s"provide ${expectedBedsWithOutgoingPatientTypeScNb} outgoing SC bed" in {
        bedsWithOutgoingPatientTypeSc must have size expectedBedsWithOutgoingPatientTypeScNb
      }
      s"provide ${expectedPatientTypeChangeFromScToSiNb} patient type change from SC to SI" in {
        patientTypeChangeFromScToSi must have size expectedPatientTypeChangeFromScToSiNb
      }
      s"provide ${expectedPatientTypeChangeFromSiToScNb} patient type change from SI to SC" in {
        patientTypeChangeFromSiToSc must have size expectedPatientTypeChangeFromSiToScNb
      }
      s"provide ${expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb} tranfer type change for SI" in {
        bedsWithTransferTypeOnlyChangePatientTypeSi must have size expectedBedsWithTransferTypeOnlyChangePatientTypeSiNb
      }
      s"provide ${expectedBedsWithTransferTypeOnlyChangePatientTypeScNb} tranfer type change for SC" in {
        bedsWithTransferTypeOnlyChangePatientTypeSc must have size expectedBedsWithTransferTypeOnlyChangePatientTypeScNb
      }
    }

  }

}
