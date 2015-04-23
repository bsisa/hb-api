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

/**
 * Tests HospitalHelperForCdfSpec
 * 
 * Warning: `Simulation` test MUST be sequential to behave correctly
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.patman.simulations.model.HospitalHelperForCdfSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class HospitalHelperForCdfSpec extends Specification {
  // Set sequential execution
  sequential

  val TestResourcesDir = "./test/resources/"

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

  s"simulatedHospitalState after init" should {
    "be None" in {
      simulatedHospitalState must beNone
    }
  }

  /**
   * Maintained hospital aggregated figures delivered at simulation end.
   */
  var simulationSummary: Option[HospitalSimulationSummary] = None

  // Given the behaviour of spec2 to allow several expectations to test
  // a given state we need to expose the intermediary states as var. 

  var bedsWithIncomingPatientTypeSiVar: List[Bed] = Nil
  var bedsWithIncomingPatientTypeScVar: List[Bed] = Nil
  var bedsWithOutgoingPatientTypeSiVar: List[Bed] = Nil
  var bedsWithOutgoingPatientTypeScVar: List[Bed] = Nil
  var patientTypeChangeFromScToSiVar: List[Bed] = Nil
  var patientTypeChangeFromSiToScVar: List[Bed] = Nil
  var bedsWithTransferTypeOnlyChangePatientTypeSiVar: List[Bed] = Nil
  var bedsWithTransferTypeOnlyChangePatientTypeScVar: List[Bed] = Nil

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

  "Simulation" should {
    sequential

    s"simulate HS t=0 be None" in {
      println(s"starting t=0 ")
      Thread.sleep(3000)
      println(s"simulatedHospitalState t=0 = ${simulatedHospitalState}")

      simulatedHospitalState must beNone
    }

    // ==================================================================
    // 	HOSPITAL_STATE_1
    // ==================================================================

    s"Simulated HS t=1 must be an Hospital instance" in {
      println(s"starting t=1 ")
      rollHospitalStates(hospitalState1)
      updateVarsForStep()
      println(s"simulatedHospitalState t1 = ${simulatedHospitalState}")
      simulatedHospitalState must beSome[Hospital]
    }
    s"Simulated HS t=1 must contain 0 SI beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SI) must have size 0
    }
    s"Simulated HS t=1 must contain 1 SC beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC) must have size 1
    }    
    
    
    s"HospitalState 1 provides 2 incoming SI beds" in {
      bedsWithIncomingPatientTypeSiVar must have size 2
    }
    s"HospitalState 1 provides 1 incoming SC bed" in {
      bedsWithIncomingPatientTypeScVar must have size 1
    }
    s"HospitalState 1 provides 0 outgoing SI bed" in {
      bedsWithOutgoingPatientTypeSiVar must have size 0
    }
    s"HospitalState 1 provides 0 outgoing SC bed" in {
      bedsWithOutgoingPatientTypeScVar must have size 0
    }
    s"HospitalState 1 provides 0 patient type change from SC to SI" in {
      patientTypeChangeFromScToSiVar must have size 0
    }
    s"HospitalState 1 provides 0 patient type change from SI to SC" in {
      patientTypeChangeFromSiToScVar must have size 0
    }
    s"HospitalState 1 provides 0 tranfer type change for SI" in {
      bedsWithTransferTypeOnlyChangePatientTypeSiVar must have size 0
    }
    s"HospitalState 1 provides 0 tranfer type change for SC" in {
      bedsWithTransferTypeOnlyChangePatientTypeScVar must have size 0
    }

    // ==================================================================
    // 	HOSPITAL_STATE_2
    // ==================================================================    

    s"simulate HS t=2 be an Hospital instance" in {
      println(s"starting t=2 ")
      rollHospitalStates(hospitalState2)
      updateVarsForStep()
      println(s"simulatedHospitalState t2 = ${simulatedHospitalState}")
      simulatedHospitalState must beSome[Hospital]
    }
    s"Simulated HS t=2 must contain 0 SI beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SI) must have size 0
    }
    s"Simulated HS t=2 must contain 2 SC beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC) must have size 2
    }
    
    
    s"HospitalState 2 provides 1 incoming SI beds" in {
      bedsWithIncomingPatientTypeSiVar must have size 1
    }
    s"HospitalState 2 provides 1 incoming SC bed" in {
      bedsWithIncomingPatientTypeScVar must have size 1
    }
    s"HospitalState 2 provides 0 outgoing SI bed" in {
      bedsWithOutgoingPatientTypeSiVar must have size 0
    }
    s"HospitalState 2 provides 0 outgoing SC bed" in {
      bedsWithOutgoingPatientTypeScVar must have size 0
    }
    s"HospitalState 2 provides 0 patient type change from SC to SI" in {
      patientTypeChangeFromScToSiVar must have size 0
    }
    s"HospitalState 2 provides 0 patient type change from SI to SC" in {
      patientTypeChangeFromSiToScVar must have size 0
    }
    s"HospitalState 2 provides 0 tranfer type change for SI" in {
      bedsWithTransferTypeOnlyChangePatientTypeSiVar must have size 0
    }
    s"HospitalState 2 provides 0 tranfer type change for SC" in {
      bedsWithTransferTypeOnlyChangePatientTypeScVar must have size 0
    }


    // ==================================================================
    // 	HOSPITAL_STATE_3
    // ==================================================================

    s"simulate HS t=3 be an Hospital instance" in {
      println(s"starting t=3 ")
      rollHospitalStates(hospitalState3)
      updateVarsForStep()
      println(s"simulatedHospitalState t3 = ${simulatedHospitalState}")
      simulatedHospitalState must beSome[Hospital]
    }
    s"Simulated HS t=3 must contain 0 SI beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SI) must have size 0
    }
    s"Simulated HS t=3 must contain 2 SC beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC) must have size 2
    }
    
    s"HospitalState 3 provides 0 incoming SI beds" in {
      bedsWithIncomingPatientTypeSiVar must have size 0
    }
    s"HospitalState 3 provides 1 incoming SC bed" in {
      bedsWithIncomingPatientTypeScVar must have size 1
    }
    s"HospitalState 3 provides 0 outgoing SI bed" in {
      bedsWithOutgoingPatientTypeSiVar must have size 0
    }
    s"HospitalState 3 provides 1 outgoing SC bed" in {
      bedsWithOutgoingPatientTypeScVar must have size 1
    }
    s"HospitalState 3 provides 0 patient type change from SC to SI" in {
      patientTypeChangeFromScToSiVar must have size 0
    }
    s"HospitalState 3 provides 0 patient type change from SI to SC" in {
      patientTypeChangeFromSiToScVar must have size 0
    }
    s"HospitalState 3 provides 0 tranfer type change for SI" in {
      bedsWithTransferTypeOnlyChangePatientTypeSiVar must have size 0
    }
    s"HospitalState 3 provides 0 tranfer type change for SC" in {
      bedsWithTransferTypeOnlyChangePatientTypeScVar must have size 0
    }

    
    // ==================================================================
    // 	HOSPITAL_STATE_4
    //  Bed 504B New SI patient id 9997101 MED 
    // ==================================================================

    s"simulate HS t=4 be an Hospital instance" in {
      println(s"starting t=4 ")
      rollHospitalStates(hospitalState4)
      updateVarsForStep()
      println(s"simulatedHospitalState t4 = ${simulatedHospitalState}")
      simulatedHospitalState must beSome[Hospital]
    }
    s"Simulated HS t=4 must contain 0 SI beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SI) must have size 0
    }
    s"Simulated HS t=4 must contain 2 SC beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC) must have size 2
    }

    s"HospitalState 4 provides 1 incoming SI beds" in {
      bedsWithIncomingPatientTypeSiVar must have size 1
    }
    s"HospitalState 4 provides 0 incoming SC bed" in {
      bedsWithIncomingPatientTypeScVar must have size 0
    }
    s"HospitalState 4 provides 0 outgoing SI bed" in {
      bedsWithOutgoingPatientTypeSiVar must have size 0
    }
    s"HospitalState 4 provides 0 outgoing SC bed" in {
      bedsWithOutgoingPatientTypeScVar must have size 0
    }
    s"HospitalState 4 provides 0 patient type change from SC to SI" in {
      patientTypeChangeFromScToSiVar must have size 0
    }
    s"HospitalState 4 provides 0 patient type change from SI to SC" in {
      patientTypeChangeFromSiToScVar must have size 0
    }
    s"HospitalState 4 provides 0 tranfer type change for SI" in {
      bedsWithTransferTypeOnlyChangePatientTypeSiVar must have size 0
    }
    s"HospitalState 4 provides 0 tranfer type change for SC" in {
      bedsWithTransferTypeOnlyChangePatientTypeScVar must have size 0
    }    
    
    
    // ==================================================================
    // 	HOSPITAL_STATE_5
    //
	//  501
	//  9997922 SC out MED
	//  9997419 SI in SPEC
	//  
	//  504B => 502
	//  9997101 SI MED - no change
	//  
	//  503A
	//  9995826 SI to SC    pat type change
	//          SPEC to MED trn type change
	//  
	//  506
	//  9993784 SC out MED
	//  9990075 SI in  SPEC
	//  
	//  SI in    size 2
	//  SC in    size 0
	//  SI out   size 0
	//  SC out   size 2
	//  SI to SC size 1    
    // ==================================================================

    s"simulate HS t=5 be an Hospital instance" in {
      println(s"starting t=5 ")
      rollHospitalStates(hospitalState5)
      updateVarsForStep()
      println(s"simulatedHospitalState t5 = ${simulatedHospitalState}")
      simulatedHospitalState must beSome[Hospital]
    }
    s"Simulated HS t=5 must contain 0 SI beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SI) must have size 0
    }
    // The single SC bed present in hospitalState5 is a former SI patient transferred to PRT which change 
    // his patient type to SC while staying a PRT.
    s"Simulated HS t=5 must contain 0 SC beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC) must have size 0
    }
    
    s"HospitalState 5 provides 2 incoming SI beds" in {
      bedsWithIncomingPatientTypeSiVar must have size 2
    }
    s"HospitalState 5 provides 0 incoming SC bed" in {
      bedsWithIncomingPatientTypeScVar must have size 0
    }
    s"HospitalState 5 provides 0 outgoing SI bed" in {
      bedsWithOutgoingPatientTypeSiVar must have size 0
    }
    s"HospitalState 5 provides 2 outgoing SC bed" in {
      bedsWithOutgoingPatientTypeScVar must have size 2
    }
    s"HospitalState 5 provides 0 patient type change from SC to SI" in {
      patientTypeChangeFromScToSiVar must have size 0
    }
    s"HospitalState 5 provides 1 patient type change from SI to SC" in {
      patientTypeChangeFromSiToScVar must have size 1
    }
    s"HospitalState 5 provides 0 tranfer type change for SI" in {
      bedsWithTransferTypeOnlyChangePatientTypeSiVar must have size 0
    }
    s"HospitalState 5 provides 0 tranfer type change for SC" in {
      bedsWithTransferTypeOnlyChangePatientTypeScVar must have size 0
    }    
   
    
    // ==================================================================
    // 	HOSPITAL_STATE_6
    //  5 SI, 1 SC, SI transfer type change only.
    // ==================================================================

    s"simulate HS t=6 be an Hospital instance" in {
      println(s"starting t=6 ")
      rollHospitalStates(hospitalState6)
      updateVarsForStep()
      println(s"simulatedHospitalState t6 = ${simulatedHospitalState}")
      simulatedHospitalState must beSome[Hospital]
    }    
    s"Simulated HS t=6 must contain 0 SI beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SI) must have size 0
    }
    // The single SC bed present in hospitalState6 is a former SI patient transferred to PRT which change 
    // his patient type to SC while staying a PRT.
    s"Simulated HS t=6 must contain 0 SC beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC) must have size 0
    }
    
    s"HospitalState 6 provides 0 incoming SI beds" in {
      bedsWithIncomingPatientTypeSiVar must have size 0
    }
    s"HospitalState 6 provides 0 incoming SC bed" in {
      bedsWithIncomingPatientTypeScVar must have size 0
    }
    s"HospitalState 6 provides 0 outgoing SI bed" in {
      bedsWithOutgoingPatientTypeSiVar must have size 0
    }
    s"HospitalState 6 provides 0 outgoing SC bed" in {
      bedsWithOutgoingPatientTypeScVar must have size 0
    }
    s"HospitalState 6 provides 0 patient type change from SC to SI" in {
      patientTypeChangeFromScToSiVar must have size 0
    }
    s"HospitalState 6 provides 0 patient type change from SI to SC" in {
      patientTypeChangeFromSiToScVar must have size 0
    }
    s"HospitalState 6 provides 1 tranfer type change for SI" in {
      bedsWithTransferTypeOnlyChangePatientTypeSiVar must have size 1
    }
    s"HospitalState 6 provides 0 tranfer type change for SC" in {
      bedsWithTransferTypeOnlyChangePatientTypeScVar must have size 0
    }    


    // ==================================================================
    // 	HOSPITAL_STATE_7
    //  505
    //  9991663 SI to SC
    // ==================================================================

    s"simulate HS t=7 be an Hospital instance" in {
      println(s"starting t=7 ")
      rollHospitalStates(hospitalState7)
      updateVarsForStep()
      println(s"simulatedHospitalState t7 = ${simulatedHospitalState}")
      simulatedHospitalState must beSome[Hospital]
    }    
    s"Simulated HS t=7 must contain 0 SI beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SI) must have size 0
    }
    // Both SC beds present in hospitalState7 are a former SI patient transferred to PRT which changed 
    // their patient type to SC while staying a PRT (beds 503A, 505)    
    s"Simulated HS t=7 must contain 0 SC beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC) must have size 0
    }
    
    s"HospitalState 7 provides 0 incoming SI beds" in {
      bedsWithIncomingPatientTypeSiVar must have size 0
    }
    s"HospitalState 7 provides 0 incoming SC bed" in {
      bedsWithIncomingPatientTypeScVar must have size 0
    }
    s"HospitalState 7 provides 0 outgoing SI bed" in {
      bedsWithOutgoingPatientTypeSiVar must have size 0
    }
    s"HospitalState 7 provides 0 outgoing SC bed" in {
      bedsWithOutgoingPatientTypeScVar must have size 0
    }
    s"HospitalState 7 provides 0 patient type change from SC to SI" in {
      patientTypeChangeFromScToSiVar must have size 0
    }
    s"HospitalState 7 provides 1 patient type change from SI to SC" in {
      patientTypeChangeFromSiToScVar must have size 1
    }
    s"HospitalState 7 provides 0 tranfer type change for SI" in {
      bedsWithTransferTypeOnlyChangePatientTypeSiVar must have size 0
    }
    s"HospitalState 7 provides 0 tranfer type change for SC" in {
      bedsWithTransferTypeOnlyChangePatientTypeScVar must have size 0
    }    

    
    // ==================================================================
    // 	HOSPITAL_STATE_8
    //  501
    //  9997419 SI out
    // ==================================================================
    
    s"simulate HS t=8 be an Hospital instance" in {
      println(s"starting t=8 ")
      rollHospitalStates(hospitalState8)
      updateVarsForStep()
      println(s"simulatedHospitalState t8 = ${simulatedHospitalState}")
      simulatedHospitalState must beSome[Hospital]
    }    
    s"Simulated HS t=8 must contain 0 SI beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SI) must have size 0
    }
    // Both SC beds present in hospitalState7 are a former SI patient transferred to PRT which changed 
    // their patient type to SC while staying a PRT (beds 503A, 505)    
    s"Simulated HS t=8 must contain 0 SC beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC) must have size 0
    }
    
    s"HospitalState 8 provides 0 incoming SI beds" in {
      bedsWithIncomingPatientTypeSiVar must have size 0
    }
    s"HospitalState 8 provides 0 incoming SC bed" in {
      bedsWithIncomingPatientTypeScVar must have size 0
    }
    s"HospitalState 8 provides 1 outgoing SI bed" in {
      bedsWithOutgoingPatientTypeSiVar must have size 1
    }
    s"HospitalState 8 provides 0 outgoing SC bed" in {
      bedsWithOutgoingPatientTypeScVar must have size 0
    }
    s"HospitalState 8 provides 0 patient type change from SC to SI" in {
      patientTypeChangeFromScToSiVar must have size 0
    }
    s"HospitalState 8 provides 0 patient type change from SI to SC" in {
      patientTypeChangeFromSiToScVar must have size 0
    }
    s"HospitalState 8 provides 0 tranfer type change for SI" in {
      bedsWithTransferTypeOnlyChangePatientTypeSiVar must have size 0
    }
    s"HospitalState 8 provides 0 tranfer type change for SC" in {
      bedsWithTransferTypeOnlyChangePatientTypeScVar must have size 0
    }   
    
    
    // ==================================================================
    // 	HOSPITAL_STATE_9
    //  503A
    //  9995826 SC out MED
	//  504B
	//  9997437 SC in MED
    // ==================================================================    
    
    s"simulate HS t=9 be an Hospital instance" in {
      println(s"starting t=9 ")
      rollHospitalStates(hospitalState9)
      updateVarsForStep()
      println(s"simulatedHospitalState t9 = ${simulatedHospitalState}")
      simulatedHospitalState must beSome[Hospital]
    }
    s"Simulated HS t=9 must contain 0 SI beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SI) must have size 0
    }
    // A new SC beds is present in hospitalState9 in bed 504B. 
    // Former SI patient transferred to PRT which changed their patient type to SC while staying a PRT are beds 503A, 505
    // Bed 503A is now outgoing
    s"Simulated HS t=9 must contain 1 SC beds" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC) must have size 1
    }
    s"Simulated HS t=9 single SC beds must be in bed 504B" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC)(0).id mustEqual "504B"
    }
    s"Simulated HS t=9 single SC beds must have patient number 9997437" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC)(0).patientNb mustEqual "9997437"
    }
    s"Simulated HS t=9 single SC beds must have transfer type `médicalisé`" in {
      simulatedHospitalState.get.beds.filter(b => b.patientType == Constants.PATIENT_TYPE_SC)(0).transferType mustEqual Constants.TRANSFER_TYPE_MED
    }    
    
    s"HospitalState 9 provides 0 incoming SI beds" in {
      bedsWithIncomingPatientTypeSiVar must have size 0
    }
    s"HospitalState 9 provides 1 incoming SC bed" in {
      bedsWithIncomingPatientTypeScVar must have size 1
    }
    s"HospitalState 9 provides 0 outgoing SI bed" in {
      bedsWithOutgoingPatientTypeSiVar must have size 0
    }
    s"HospitalState 9 provides 1 outgoing SC bed" in {
      bedsWithOutgoingPatientTypeScVar must have size 1
    }
    s"HospitalState 9 provides 0 patient type change from SC to SI" in {
      patientTypeChangeFromScToSiVar must have size 0
    }
    s"HospitalState 9 provides 0 patient type change from SI to SC" in {
      patientTypeChangeFromSiToScVar must have size 0
    }
    s"HospitalState 9 provides 0 tranfer type change for SI" in {
      bedsWithTransferTypeOnlyChangePatientTypeSiVar must have size 0
    }
    s"HospitalState 9 provides 0 tranfer type change for SC" in {
      bedsWithTransferTypeOnlyChangePatientTypeScVar must have size 0
    }
    
  }


  /**
   * Mutates var references.
   */
  def rollHospitalStates(newHospitalState: Hospital) = {
    previousHospitalState = currentHospitalState
    currentHospitalState = Some(newHospitalState)
  }


  /**
   * Depends on previousHospitalState, currentHospitalState vars.
   * Returns Unit.
   * Has side effect on all tested vars.
   */
  def updateVarsForStep() = {
    HospitalHelper.getBedsUpdates(previousHospitalState, currentHospitalState) match {
      case (
        bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
        bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
        patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
        bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc) =>

        bedsWithIncomingPatientTypeSiVar = bedsWithIncomingPatientTypeSi
        bedsWithIncomingPatientTypeScVar = bedsWithIncomingPatientTypeSc
        bedsWithOutgoingPatientTypeSiVar = bedsWithOutgoingPatientTypeSi
        bedsWithOutgoingPatientTypeScVar = bedsWithOutgoingPatientTypeSc
        patientTypeChangeFromScToSiVar = patientTypeChangeFromScToSi
        patientTypeChangeFromSiToScVar = patientTypeChangeFromSiToSc
        bedsWithTransferTypeOnlyChangePatientTypeSiVar = bedsWithTransferTypeOnlyChangePatientTypeSi
        bedsWithTransferTypeOnlyChangePatientTypeScVar = bedsWithTransferTypeOnlyChangePatientTypeSc

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
        simulatedHospitalState = HospitalHelper.updateSimulatedHospitalStateForCdf(
          currentSimulatedHospitalStateOption = simulatedHospitalState,
          newStaticHospitalStateOption = currentHospitalState,
          bedsWithIncomingPatientTypeSi, bedsWithIncomingPatientTypeSc,
          bedsWithOutgoingPatientTypeSi, bedsWithOutgoingPatientTypeSc,
          patientTypeChangeFromScToSi, patientTypeChangeFromSiToSc,
          bedsWithTransferTypeOnlyChangePatientTypeSi, bedsWithTransferTypeOnlyChangePatientTypeSc)
    }
  }

}
