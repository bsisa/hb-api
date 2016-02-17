package ch.bsisa.hyperbird.report

import ch.bsisa.hyperbird.Implicits._
import scala.sys.process._

/**
 *
 * Wrapper around external command line tool `pdftk`
 * This utility is used in the context of reporting.
 *
 * @author Patrick Refondini
 *
 */
object PdfFileMergingHelper {

  private val CmdInstruction = "output"

  /**
   * Merges the list of files identified by `inputFilesPath` to a single file 
   * identified by `outputFilePath`.
   * 
   * The file paths must be a fully qualified path to file.
   *   
   * This function blocks until the merge process has completed thus guaranteeing
   * access to generated output file found at `outputFilePath`.
   */
  def mergePdfFiles(inputFilesPath: Seq[String], outputFilePath: String)(implicit reportConfig: ReportConfig) : Int = {

    val cmdSeq = reportConfig.pdfMergingPath +: inputFilesPath :+ CmdInstruction :+ outputFilePath

    // Using .! blocking call is deliberate. Using non blocking .run here 
    // would break this function contract which guarantees output file 
    // is accessible upon successful function completion (exitCode == 0). 
    val exitCode = Process(cmdSeq).!
    exitCode
  }

}