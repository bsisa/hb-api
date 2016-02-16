package test.ch.bsisa.hyperbird.report

import ch.bsisa.hyperbird.Implicits._
import test.ch.bsisa.hyperbird.util.BaseSerialisationSpec
import ch.bsisa.hyperbird.report.PdfFileMergingHelper
import java.io.File
import org.specs2.mutable._

import play.api.test._

import play.api.test.Helpers._

import play.api.GlobalSettings
//import play.api.WithDefaultConfiguration

/**
 * Tests ch.bsisa.hyperbird.io.PdfFileMergingHelper.mergePdfFiles() method
 * This method is used to merge several PDF files into a single one.  
 * Input files content appears in the provide input files order.
 * No modification of input files is performed, paging is kept "as is".
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.io.PdfFileMergingHelperSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
//class PdfFileMergingHelperSpec extends BaseSerialisationSpec with PlaySpecification {
class PdfFileMergingHelperSpec extends BaseSerialisationSpec  with PlaySpecification {

  val sourceDirectory = new File(TestResourcesDir)
  val targetDirectory = new File(TestResultsDir)

  val outputFileName = "MergedFileFromPdfFileMerging.pdf"
  val outputFile = new File(new File(TestResultsDir), outputFileName)
  val outputFileAbsPathName = outputFile.getCanonicalPath
  
  val sourceFileName1 = "pdfFilesMergingInput1.pdf"
  val sourceFileName2 = "pdfFilesMergingInput2.pdf"
  val sourceFileName3 = "pdfFilesMergingInput3.pdf"
  
  def buildInputFilesAbsolutePathNameList (names : Seq[String]) : Seq[String] = {
    val absPath = for ( name <- names) yield { new File(sourceDirectory, name).getCanonicalPath }
    absPath
  } 
  
  def getSumOfFileSize(filesAbsPath : Seq[String]) : Long = {
    val sizes = for (fileAbsPath <- filesAbsPath) yield {
      new File(fileAbsPath).length()
    }
    sizes.sum
  }
  
  val inputFilesAbsPathNameList = buildInputFilesAbsolutePathNameList(Seq(sourceFileName1, sourceFileName2, sourceFileName3))
  Console println s">>>>> INPUTs: ${inputFilesAbsPathNameList}"
  Console println s">>>>> OUTPUT: ${outputFileAbsPathName}"
  
  // Clean up result test data if any
  if (outputFile.exists()) outputFile.delete()

  // Perform merge operation
  
  "Test PDF files merging" should {
    s"return process exitValue 0" in new WithApplication {
      val exitValue = PdfFileMergingHelper.mergePdfFiles(inputFilesAbsPathNameList, outputFileAbsPathName)
      exitValue == 0
    }
  }
  
//  s"The merge PDF file process exitValue" should {
//   "equal 0" in {
//     exitValue == 0
//   }
//  }
  
  // Heuristic value observed on two distinct tests. Change it as needed.
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