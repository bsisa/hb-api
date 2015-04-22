package test.ch.bsisa.hyperbird.patman.simulations.model

import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import org.specs2._
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
 * Shows Acceptance test style with sequential configuration.
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.patman.simulations.model.SequentialAcceptanceTestSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class SequentialAcceptanceTestSpec extends Specification {
  def is = sequential ^ s2"""
  
  The sharedList should
    be Nil 							$t1
    be List("hello") 				$t2 
    be List("hello","dude") 		$t3
    								"""

  var sharedList: List[String] = List()

  def t1() = {
    println(s"sharedList = ${sharedList}")
    sharedList mustEqual Nil
  }
  def t2() = {
    sharedList = List("hello")
    println(s"sharedList = ${sharedList}")
    sharedList mustEqual List("hello")
  }
  def t3() = {
    sharedList = sharedList :+ "dude"
    println(s"sharedList = ${sharedList}")
    sharedList mustEqual List("hello", "dude")
  }

}




