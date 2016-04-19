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

  val WkhtmltopdfPathKey = "hb.report.wkhtmltopdf.path"
  val PdfMergingPathKey = "hb.report.pdfmerging.path"

  /**
   * Header message is an optional string message configured on a report configuration basis (static). 
   * It is passed to configured header template as message template parameter. 
   */
  val CAR_NAME_HEADER_MESSAGE = "headerMessage"
  

  /**
   * Page orientation is an optional string message configured on a report configuration. 
   * It defines the report default page orientation. 
   * If not available `portrait` layout is used. 
   */
  val CAR_NAME_PAGE_ORIENTATION = "pageOrientation"  
  
  /**
   * Page orientation `landscape` is currently the only page orientation accepted value.
   * Any other value will be considered `portrait` page orientation.
   */
  val CAR_VALUE_PAGE_ORIENTATION_LANDSCAPE = "landscape"  
  
  
  /**
   * PDF include first is a string containing a HB triplet uniquely identifying an ELFIN object (IDG/CLASS/Id)
   * This object first ANNEX document in PDF format will be used for PDF merging at first position. 
   */  
  val CAR_NAME_PDF_INCLUDE_FIRST = "pdfIncludeFirst"
  
  /**
   * PDF include last is a string containing a HB triplet uniquely identifying an ELFIN object (IDG/CLASS/Id)
   * This object first ANNEX document in PDF format will be used for PDF merging at last position. 
   */
  val CAR_NAME_PDF_INCLUDE_LAST = "pdfIncludeLast"
  
  /**
   * Watermark Elfin is a string containing a HB triplet uniquely identifying an ELFIN object (IDG/CLASS/Id)
   * This object first ANNEX document in HTML supported image format (SVG preferred) is meant to be used 
   * as image background in PDF reports.
   * Common usage are: 'This is a draft', 'Not validated', 'Confidential',...
   */
  val CAR_NAME_WATERMARK_ELFIN_REF = "watermarkElfin"
 
}