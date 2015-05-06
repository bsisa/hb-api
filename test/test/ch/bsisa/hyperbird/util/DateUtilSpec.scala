package test.ch.bsisa.hyperbird.util

import org.specs2.mutable._
import ch.bsisa.hyperbird.util.DateUtil

/**
 * Tests DateUtilSpec
 *
 *  Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.util.DateUtilSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class DateUtilSpec extends Specification {

  val time0 = ""
  //   			=>  0h 0min  0sec 0millis

  val time1 = "14"
  //  			=> 14h 0min  0sec 0millis

  val time2 = "14:01"
  //			=> 14h 1min  0sec 0millis

  val time3 = "14:01:23"
  //			=> 14h 1min 23sec 0millis

  val time4 = "14:01:23:2"
  //			=> 14h 1min 23sec 2millis

  val brokenTime1 = "14:xx:23:2"
  //			=> None
    
  val brokenTime2 = "14:7777:23:2"
  //			=> None    
    
  s"DateUtil.parseTime" should {
    s"return None for time0 = '${time0}'" in {
      DateUtil.parseTime(time0) mustEqual None
    }
    s"return Some(Some(14), None, None, None) for time1 = '${time1}'" in {
      DateUtil.parseTime(time1) mustEqual Some(Some(14), None, None, None)
    }
    s"return Some(Some(14), Some(1), None, None) for time2 = '${time2}'" in {
      DateUtil.parseTime(time2) mustEqual Some(Some(14), Some(1), None, None)
    }
    s"return Some(Some(14), Some(1), Some(23), None) for time3 = '${time3}'" in {
      DateUtil.parseTime(time3) mustEqual Some(Some(14), Some(1), Some(23), None)
    }
    s"return Some(Some(14), Some(1), Some(23), Some(2)) for time4 = '${time4}'" in {
      DateUtil.parseTime(time4) mustEqual Some(Some(14), Some(1), Some(23), Some(2))
    }
    s"return None for brokenTime1 = '${brokenTime1}'" in {
      DateUtil.parseTime(brokenTime1) mustEqual None
    }
    s"return Some(Some(14), Some(7777), Some(23), Some(2)) for brokenTime2 = '${brokenTime2}'" in {
      DateUtil.parseTime(brokenTime2) mustEqual Some(Some(14), None, None, None)
    }    
    
  }

}