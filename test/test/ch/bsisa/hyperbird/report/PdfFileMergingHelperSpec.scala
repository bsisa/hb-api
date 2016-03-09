package test.ch.bsisa.hyperbird.report

import ch.bsisa.hyperbird.Implicits._
import test.ch.bsisa.hyperbird.util.BaseSerialisationSpec
import ch.bsisa.hyperbird.report.PdfFileMergingHelper
import java.io.File
import org.specs2.mutable._

import play.api.Configuration

import play.api.test._
import play.api.test.Helpers._


/**
 * Tests ch.bsisa.hyperbird.io.PdfFileMergingHelper.mergePdfFiles() method
 * This method is used to merge several PDF files into a single one.  
 * Input files content appears in the provide input files order.
 * No modification of input files is performed, paging is kept "as is".
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.report.PdfFileMergingHelperSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class PdfFileMergingHelperSpec extends BaseSerialisationSpec with PlaySpecification {

  import scala.collection.JavaConversions._
  
  /**
   * Obtain "hb.report.pdfmerging.path" configuration from system environment 
   * variable `"HB_REPORT_PDFMERGING_PATH"` and use it for fake application.
   * 
   * This allow easy configuration in CI environment such as Jenkins.  
   */
  val pdfmergingpathEnv = Option(System.getenv("HB_REPORT_PDFMERGING_PATH")) 
  println(s"HB_REPORT_PDFMERGING_PATH = ${pdfmergingpathEnv}")
    
  val fakeAppWithPdfMergeConfig = pdfmergingpathEnv match {
    case Some(value) => FakeApplication(
      additionalConfiguration = Map(ch.bsisa.hyperbird.report.ReportConfig.PdfMergingPathKey -> value)
      )
    case None => FakeApplication()
  }

  
  val sourceDirectory = new File(TestResourcesDir)
  val targetDirectory = new File(TestResultsDir)

  val outputFileName = "MergedFileFromPdfFileMerging.pdf"
  
  val sourceFileName1 = "pdfFilesMergingInput1.pdf"
  val sourceFileName2 = "pdfFilesMergingInput2.pdf"
  val sourceFileName3 = "pdfFilesMergingInput3.pdf"
  
  // Quick helper to build abs path sequence
  def buildInputFilesAbsolutePathNameList (names : Seq[String]) : Seq[String] = {
    val absPathSeq = for ( name <- names) yield { new File(sourceDirectory, name).getCanonicalPath }
    absPathSeq
  } 
  
  // Quick helper to compute several files size
  def getSumOfFileSize(filesAbsPath : Seq[String]) : Long = {
    val sizes = for (fileAbsPath <- filesAbsPath) yield {
      new File(fileAbsPath).length()
    }
    sizes.sum
  }

  
  // Create output file to contain merged PDF input files
  val outputFile = new File(new File(TestResultsDir), outputFileName)
  
  // Clean up previous test result data if any
  if (outputFile.exists()) outputFile.delete()  
  
  
  // Build input files abs path sequence 
  val inputFilesAbsPathNameList = buildInputFilesAbsolutePathNameList(Seq(sourceFileName1, sourceFileName2, sourceFileName3))
  // Get output file abs path
  val outputFileAbsPathName = outputFile.getCanonicalPath  

  // Perform merge operation
  "Test PDF files merging" should {
    s"return process exitValue 0" in new WithApplication(fakeAppWithPdfMergeConfig) {
      val exitValue = PdfFileMergingHelper.mergePdfFiles(inputFilesAbsPathNameList, outputFileAbsPathName)
      exitValue == 0
    }
  }
  
  // Heuristic value observed from two distinct tests. 
  // No particular meaning, only makes test a bit less
  // sensitive to input files modification. Update it as needed.
  val mergerSizeGain = 1772
  val expectedMergedFileSize = getSumOfFileSize(inputFilesAbsPathNameList) - mergerSizeGain
  
  s"The outputFile at ${outputFile.getCanonicalPath} " should {
    s"exist" in {
      outputFile.exists()
    }
    s"have size equal to ${expectedMergedFileSize}" in {
      outputFile.length() === expectedMergedFileSize
    }
  }

}