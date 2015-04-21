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
 * Tests HospitalHelper
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.patman.simulations.model.SequentialTestSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class SequentialTestSpec extends Specification {
  // Set sequential execution
  sequential  

  var sharedList : List[String] = List()
  
  "sharedList" should {
    "be None" in {
      println(s"sharedList = ${sharedList}")
      sharedList mustEqual Nil
    }
    """be List("hello")""" in {
      sharedList = List("hello")
      Thread.sleep(3000)
      println(s"sharedList = ${sharedList}")
      sharedList mustEqual List("hello")
    }
    """be List("hello","dude")""" in {
      sharedList = sharedList :+ "dude"
      Thread.sleep(3000)
      println(s"sharedList = ${sharedList}")
      sharedList mustEqual List("hello","dude")
    }    
  }


}
