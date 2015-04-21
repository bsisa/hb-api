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
    val EXPECTED_SCHEDULE_STR = "2015-01-14T08:00:00+01:00" 
  
  val simulationsTestResourcesPath = TestResourcesDir + "simulations/"
  val hospitalState1ElfinFile = "HOSPITAL_STATE_1.xml"

  // XML file => scala.xml.Elem
  // ==================================================================    
  val hospitalState1Xml = XML.loadFile(simulationsTestResourcesPath + hospitalState1ElfinFile)
  // scala.xml.Elem => ch.bsisa.hyperbird.model.ELFIN
  // ==================================================================
  val elfin = scalaxb.fromXML[ELFIN](hospitalState1Xml)

  //  var previousHospitalState: Option[Hospital] = None
  //  var currentHospitalState: Option[Hospital] = None

  // Convert generic ELFIN data structure of CLASSE='HOSPITAL_STATE' to semantic type Hospital 
  val hospitalState1 = HospitalHelper.toHospital(elfin)

  
  "The hospitalState1" should {
    s"have code '${EXPECTED_CDF_CODE}'" in {
      hospitalState1.code mustEqual EXPECTED_CDF_CODE
    }
    s"have schedule '${EXPECTED_SCHEDULE_STR}'" in {
      hospitalState1.schedule mustEqual DateUtil.getIsoDateFormatterWithoutTz.parse(EXPECTED_SCHEDULE_STR)
    }
    s"have 10 beds" in {
      hospitalState1.beds must have size(10)
    }
    s"have 3 busy beds" in {
      hospitalState1.beds.filterNot(b => b.free) must have size(3)
    }
    s"have 7 free beds" in {
      hospitalState1.beds.filter(b => b.free) must have size(7)
    }    
  }
  
  // Roll hospital states
  //      previousHospitalState = currentHospitalState
  //      currentHospitalState = Some(hospital)  

  val bed501 = Bed("501", false, "4267921", "soins continus", "médicalisé", Some("SC"))
  val bed506 = Bed("506", false, "4303782", "soins continus", "médicalisé", Some("SC"))

  val bedList1 = List(bed501, bed506)

  HospitalHelper

  //  "The 'Hello world' string" should {
  //    "contain 11 characters" in {
  //      "Hello world" must have size(11)
  //    }
  //    "start with 'Hello'" in {
  //      "Hello world" must startWith("Hello")
  //    }
  //    "end with 'world'" in {
  //      "Hello world" must endWith("world")
  //    }
  //  }

  //  s"The ANNEXEJsonFilePath at ${ANNEXEJsonFilePath} " should {
  //    s"convert to ch.bsisa.hyperbird.model.ANNEXE" in {
  //      annexeFromFile.RENVOI(0).POS must be equalTo (1)
  //      annexeFromFile.RENVOI(1).POS must be equalTo (2)
  //    }
  //  }

}