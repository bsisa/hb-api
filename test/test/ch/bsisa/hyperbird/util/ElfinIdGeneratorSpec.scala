package test.ch.bsisa.hyperbird.util

import org.specs2.mutable._
import ch.bsisa.hyperbird.util.ElfinIdGenerator
import ch.bsisa.hyperbird.util.ElfinIdGenerator.ElfinUniqueIdValidationException

import play.api.test._
import play.api.test.Helpers._

/**
 * Tests ElfinIdGenerator Id generation and validation.
 *
 * Note regarding validation:
 *
 * geoXml.xsd comments specifies the ELFIN.Id and ELFIN.ID_G content should
 * have the following format: "G" followed by 17 digits.
 *
 * The 17 digits should correspond to date time pattern: (AAAAMMJJHHMMSSmmm) (french)
 * corresponding to pattern `yyyyMMddHHmmssSSS` defined by `java.text.SimpleDateFormat`
 *
 * This last assumption does not hold true with existing data. Some identifiers
 * have been found having the following value `G20040203114894000` as defined in
 * `validUniqueIdentifierTest3` where 114894000 does not lead to a valid time value:
 * 11:48:94:000
 *
 * To keep the system functional with existing value the current latest assumption
 * is dropped from validation constraints. Thus while validation date time
 * is not enforced validUniqueIdentifierTest3 related test(s) should be enabled and
 * invalidUniqueIdentifierTest5 related test disabled.
 *
 *
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.util.ElfinIdGeneratorSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class ElfinIdGeneratorSpec extends BaseSerialisationSpec with PlaySpecification {

  val validUniqueIdentifierTest1 = "G20140210140946909"
  val validUniqueIdentifierTest2 = "G20040930101030005"
  // 114894 is not a valid time 11h48min94seconds, seconds must be 0-60
  val validUniqueIdentifierTest3 = "G20040203114894000"

  val invalidUniqueIdentifierTest4 = "20140210140946909"
  val invalidUniqueIdentifierTest5 = validUniqueIdentifierTest3
  val invalidUniqueIdentifierTest6 = "G20140210140946909G20140210140946909"

// DONE - Keep this as reminder spec override.
//    
//  override def is = s2"""
//
// This specification re-implementation is PENDING.
//  
//  Indeed the new ElfinIdGenerator.getNewElfinId(): Future[String] signature has changed 
//  from `String` to `Future[String]` 
//  and the underlying infrastructure is now Akka Actor based to deliver threadsafe access
//  to a counter.
//  
//  The current specification will have to startup an Akka system with IdActor to re-enable
//  tests to run.
//                                                                 """
   
    
  s"The ElfinIdGenerator.validateElfinUniqueIdentifier" should {
    s"validate ${validUniqueIdentifierTest1}" in new WithApplication {
      ElfinIdGenerator.validateElfinUniqueIdentifier(validUniqueIdentifierTest1) must not(throwA[ElfinUniqueIdValidationException])
    }
    s"validate ${validUniqueIdentifierTest2}" in new WithApplication {
      ElfinIdGenerator.validateElfinUniqueIdentifier(validUniqueIdentifierTest2) must not(throwA[ElfinUniqueIdValidationException])
    }
    s"validate ${validUniqueIdentifierTest3}" in new WithApplication {
      ElfinIdGenerator.validateElfinUniqueIdentifier(validUniqueIdentifierTest3) must not(throwA[ElfinUniqueIdValidationException])
    }

    s"throw an ElfinUniqueIdValidationException with value ${invalidUniqueIdentifierTest4} missing leading 'G' character" in new WithApplication {
      ElfinIdGenerator.validateElfinUniqueIdentifier(invalidUniqueIdentifierTest4) must throwA[ElfinUniqueIdValidationException]
    }
// Constraint dropped due to legacy non-compliant data 
//    s"throw an ElfinUniqueIdValidationException with value ${invalidUniqueIdentifierTest5} date part contains invalid time 94 seconds value" in new WithApplication {
//      ElfinIdGenerator.validateElfinUniqueIdentifier(invalidUniqueIdentifierTest5) must throwA[ElfinUniqueIdValidationException]
//    }
    s"throw an ElfinUniqueIdValidationException with value ${invalidUniqueIdentifierTest6} contains twice the same value" in new WithApplication {
      ElfinIdGenerator.validateElfinUniqueIdentifier(invalidUniqueIdentifierTest6) must throwA[ElfinUniqueIdValidationException]
    }
  }

}