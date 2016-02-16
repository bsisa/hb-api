package ch.bsisa.hyperbird.report

import play.api.Play

class ReportConfig {
  
  /**
   * Wkhtmltopdf binary path
   */
  lazy val wkhtmltopdfPath: String = Play.current.configuration.getString(ReportConfig.WkhtmltopdfPathKey) match {
    case Some(path) => path
    case None => throw ReportConfigException(s"Report HTML to PDF tool path configuration information ${ReportConfig.WkhtmltopdfPathKey} missing")
  }

  /**
   * PDF merging binary path
   */
  lazy val pdfMergingPath: String = Play.current.configuration.getString(ReportConfig.PdfMergingPathKey) match {
    case Some(path) => path
    case None => throw ReportConfigException(s"Report PDF merging tool path configuration information ${ReportConfig.PdfMergingPathKey} missing")
  }  
  
}

/**
 * Collections configuration exception class
 */
case class ReportConfigException(message: String = null, cause: Throwable = null) extends Exception(message, cause)


object ReportConfig {

  private val WkhtmltopdfPathKey = "hb.report.wkhtmltopdf.path"
  private val PdfMergingPathKey = "hb.report.pdfmerging.path"

}