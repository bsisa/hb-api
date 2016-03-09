package test.ch.bsisa.hyperbird.util

import org.specs2.mutable._

/**
 * Base parent test class to have some test constants
 * or utilities defined in a single place.
 *
 *  @author Patrick Refondini
 */
class BaseSerialisationSpec extends Specification {

  val TestResourcesDir = "./test/resources/"
  val TestResultsDir = "./target/test/results/"

  // Check whether TestResultsDir exists and creates it otherwise
  val testResultDir = new java.io.File(TestResultsDir)
  if (!testResultDir.exists()) { testResultDir.mkdirs() }

}
