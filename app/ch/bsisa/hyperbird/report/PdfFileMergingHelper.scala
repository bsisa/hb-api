package ch.bsisa.hyperbird.report

import play.api.Play

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

    println(s">>>> cmdSeq = " + cmdSeq.toString().substring(5).replaceAll(",", " "))
    
    // Using .! blocking call is deliberate. Using non blocking .run here 
    // would break this function contract which guarantees output file 
    // is accessible upon successful function completion (exitCode == 0). 
    val exitCode = Process(cmdSeq).!
    exitCode
  }

}

object PdfFileWatermarkHelper {
  private val CmdInstruction = "background"
  private val OutputInstruction = "output"

  def stampPdfFile(inputFilePath: String, outputFilePath: String, watermarkName:String)(implicit reportConfig: ReportConfig) : Int = {

    val watermarkResource = s"/conf/resources/reports/additions/$watermarkName.pdf"
    val stamp = Play.current.getFile(watermarkResource).getAbsolutePath
    val cmdSeq = reportConfig.pdfMergingPath +: Seq(inputFilePath) :+ CmdInstruction :+ stamp :+ OutputInstruction :+ outputFilePath

    println(s">>>> cmdSeq = " + cmdSeq.toString().substring(5).replaceAll(",", " "))

    Process(cmdSeq).!
  }
}