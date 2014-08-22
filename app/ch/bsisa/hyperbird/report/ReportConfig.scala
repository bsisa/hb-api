package ch.bsisa.hyperbird.report

import play.api.Play

class ReportConfig {
  /**
   * Wkhtmltopdf Binary Path
   */
  val wkhtmltopdfPath: String = Play.current.configuration.getString(ReportConfig.WkhtmltopdfPathKey) match {
    case Some(path) => path
    case None => throw ReportConfigException(s"Report path identifier information ${ReportConfig.WkhtmltopdfPathKey} missing")
  }

}

/**
 * Collections configuration exception class
 */
case class ReportConfigException(message: String = null, cause: Throwable = null) extends Exception(message, cause)


object ReportConfig {

  private val WkhtmltopdfPathKey = "hb.report.wkhtmltopdf.path"

}