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

  def renderTemplate(templateName: String, xml: Elem): String = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val template = m.reflectModule(m.staticModule(templateName + "$")).instance.asInstanceOf[Template1[Elem, Result]]

    template.render(xml).toString
  }

  def writeReport(reportElfin: ELFIN, queryString: Option[String])(implicit reportConfig: ReportConfig): Future[TemporaryFile] = {

    // ==============================================================
    // Extract parameters from reportElfin
    // ==============================================================    
    val headerTemplateName = reportElfin.CARACTERISTIQUE.get.CAR1.get.VALEUR.get
    val contentTemplateName = reportElfin.CARACTERISTIQUE.get.CAR2.get.VALEUR.get
    val footerTemplateName = reportElfin.CARACTERISTIQUE.get.CAR3.get.VALEUR.get
    val queryFileName = reportElfin.CARACTERISTIQUE.get.CAR4.get.VALEUR.get
    val reportFileNamePrefix = reportElfin.CARACTERISTIQUE.get.CAR5.get.VALEUR.get
    // URL encoding is necessary for query content but not for file name
    //val queryFileName = URLEncoder.encode(reportElfin.DIVERS.get.METHODE.get, "UTF-8")
    //val queryFileName = reportElfin.DIVERS.get.METHODE.get

    // ==============================================================
    // Run XQuery by file name (TODO: review if wrapped is what we want.)
    // ==============================================================
    //val responseFuture = XQueryWSHelper.runWrappedXQueryFile(queryFileName.trim, queryString)
    val responseFuture = XQueryWSHelper.runXQueryFile(queryFileName.trim, queryString)

    responseFuture.map { response =>
      val respBody = response.body
      val resultData = XML.loadString(respBody)
//      println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
//      println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
//      println(respBody)
//      println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
//      println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
      
      // Render report header to HTML and save it to disk
      val reportHeaderHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportHeader", ".html"))
      play.api.libs.Files.writeFile(reportHeaderHtmlTempFile.file, renderTemplate(headerTemplateName, resultData))
      
      // Render report footer to HTML and save it to disk
      val reportFooterHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportFooter", ".html"))
      play.api.libs.Files.writeFile(reportFooterHtmlTempFile.file, renderTemplate(footerTemplateName, resultData))      

      // Render report body to HTML and save it to disk
      val reportContentHtmlString = renderTemplate(contentTemplateName, resultData)

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

