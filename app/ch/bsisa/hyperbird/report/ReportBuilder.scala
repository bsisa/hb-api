package ch.bsisa.hyperbird.report

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.ws.{ WSQueries, XQueryWSHelper }
import ch.bsisa.hyperbird.model.ELFIN

import io.github.cloudify.scala.spdf.{ Portrait, PdfConfig, Pdf }
import org.apache.commons.codec.binary.Base64

import play.api.Play
import play.api.libs.Files.TemporaryFile
import play.api.mvc.Result
import play.api.templates.Template1
import play.api.templates.Template2

import java.io.{ InputStream }
import java.net.URLEncoder
import java.text.DecimalFormat

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.xml.{ Elem, XML }
import scala.xml.PrettyPrinter

/**
 * PDF report builder based on wkhtmltopdf 
 */
object ReportBuilder {

  /**
   * Renders template by name to String for wkhtmltopdf to process  
   */
  def renderTemplate[A](templateName: String, data: A, reportTitle: String): String = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val template = m.reflectModule(m.staticModule(templateName + "$")).instance.asInstanceOf[Template2[A, String, Result]]

    template.render(data, reportTitle).toString
  }  
  

  /**
   * Produces a PDF report given a report description `reportElfin`in geoXml ELFIN format 
   * and optional raw HTTP GET query string to provide additional dynamic parameters to 
   * XQuery referred to in ELFIN report description.
   *
   * Example report description in ELFIN format:  
		<ELFIN Id="xxx" ID_G="xxx" CLASSE="RAPPORT" GROUPE="" TYPE="ACTIVITE" NATURE="Flux">
			(...)
		    <CARACTERISTIQUE>
		        <CAR1 NOM="header" UNITE="reference" VALEUR="views.html.reports.defaultHeader"/>
		        <CAR2 NOM="content" UNITE="reference" VALEUR="views.html.reports.defaultBody"/>
		        <CAR3 NOM="footer" UNITE="reference" VALEUR="views.html.reports.defaultFooter"/>
		        <CAR4 NOM="query" UNITE="reference" VALEUR="myReportXQuery.xq"/>
		        <CAR5 NOM="filename" UNITE="name" VALEUR="FriendlyFileNamePrefix"/>
		        <CAR6 NOM="reportTitle" UNITE="name" VALEUR="The Report Title"/>
		        <CALCUL/>
		    </CARACTERISTIQUE>
			(...)
		</ELFIN>  
   *    
   */
  def writeReport(reportElfin: ELFIN, queryString: Option[String])(implicit reportConfig: ReportConfig): Future[TemporaryFile] = {

    // ==============================================================
    // Extract parameters from reportElfin
    // ==============================================================    
    val headerTemplateName = reportElfin.CARACTERISTIQUE.get.CAR1.get.VALEUR.get
    val contentTemplateName = reportElfin.CARACTERISTIQUE.get.CAR2.get.VALEUR.get
    val footerTemplateName = reportElfin.CARACTERISTIQUE.get.CAR3.get.VALEUR.get
    val queryFileName = reportElfin.CARACTERISTIQUE.get.CAR4.get.VALEUR.get
    val reportFileNamePrefix = reportElfin.CARACTERISTIQUE.get.CAR5.get.VALEUR.get
    val reportTitle = reportElfin.CARACTERISTIQUE.get.CAR6.get.VALEUR.get
    // URL encoding is necessary for query content but not for file name
    //val queryFileName = URLEncoder.encode(reportElfin.DIVERS.get.METHODE.get, "UTF-8")

    // ==============================================================
    // Run XQuery by file name
    // ==============================================================
    val responseFuture = XQueryWSHelper.runXQueryFile(queryFileName.trim, queryString)

    responseFuture.map { response =>
      // Work with String expected to contain HTML.
      val resultData = response.body
      // XML data unused at the moment.
      //val resultData = XML.loadString(respBody)
      
      // Render report header to HTML and save it to disk
      val reportHeaderHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportHeader", ".html"))
      play.api.libs.Files.writeFile(reportHeaderHtmlTempFile.file, renderTemplate(headerTemplateName, resultData, reportTitle))
      
      // Render report footer to HTML and save it to disk
      val reportFooterHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportFooter", ".html"))
      play.api.libs.Files.writeFile(reportFooterHtmlTempFile.file, renderTemplate(footerTemplateName, resultData, reportTitle))      

      // Render report body to HTML and save it to disk
      val reportContentHtmlString = renderTemplate(contentTemplateName, resultData, reportTitle)

      // Configure wkhtmltopdf 
      val pdf = Pdf(
        reportConfig.wkhtmltopdfPath,
        new PdfConfig {
          orientation := Portrait
          pageSize := "A4"
          headerHtml := reportHeaderHtmlTempFile.file.getAbsolutePath
          footerHtml := reportFooterHtmlTempFile.file.getAbsolutePath
        })
      // Create empty temporary file for final PDF report outcome.
      val tempResult = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))
      // Process HTML temporary files to PDF using wkhtmltopdf
      pdf.run(reportContentHtmlString, tempResult.file)

      tempResult
    }
  }

  def toFormattedNumber(text: String): String = {

    try {
      val formatter = new DecimalFormat("###,###.##")
      val value = java.lang.Double.valueOf(text)
      formatter.format(value)
    } catch {
      case _: Throwable => ""

    }

  }

  def encodeImage(path: String, mimeType: String): String = {
    val isOption: Option[InputStream] = Play.current.resourceAsStream(path)

    isOption match {
      case Some(is) =>
        val bytes = new Array[Byte](is.available())
        is.read(bytes)
        val base64: Base64 = new Base64()
        // data:image/gif;base64,
        "data:" + mimeType + ";base64," + base64.encodeAsString(bytes)

      case None =>
        ""
    }
  }

}

