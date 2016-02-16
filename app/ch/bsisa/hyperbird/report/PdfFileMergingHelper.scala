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
   * Merges the list of files identified by `inputFilesPath` to a single file identified by `outputFilePath`.
   * The file paths must be a fully qualified path to file.  
   */
  def mergePdfFiles(inputFilesPath: Seq[String], outputFilePath: String)(implicit reportConfig: ReportConfig) : Int = {

    val cmdSeq = reportConfig.pdfMergingPath +: inputFilesPath :+ CmdInstruction :+ outputFilePath

    // .! is a blocking call, we may use .run instead if performance is a concern
    val exitCode = Process(cmdSeq).!
    exitCode
  }

}