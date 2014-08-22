package ch.bsisa.hyperbird.report

import java.io.{InputStream}
import java.net.URLEncoder
import java.text.DecimalFormat
import scala.concurrent.Future

import ch.bsisa.hyperbird.dao.ws.{WSQueries, XQueryWSHelper}
import ch.bsisa.hyperbird.model.ELFIN
import io.github.cloudify.scala.spdf.{Portrait, PdfConfig, Pdf}

import org.apache.commons.codec.binary.Base64
import play.api.Play
import play.api.libs.Files.TemporaryFile
import play.api.mvc.Result
import play.api.templates.Template1

import scala.concurrent.ExecutionContext.Implicits.global

import scala.xml.{Elem, XML}


object ReportBuilder {


  def renderTemplate(templateName:String, xml:Elem):String = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val template = m.reflectModule(m.staticModule(templateName + "$")).instance.asInstanceOf[Template1[Elem, Result]]

    template.render(xml).toString
  }

  def writeReport(reportElfin:ELFIN)(implicit reportConfig:ReportConfig): Future[TemporaryFile] = {


    val headerTemplateName = reportElfin.CARACTERISTIQUE.get.CAR1.get.VALEUR.get
    val contentTemplateName = reportElfin.CARACTERISTIQUE.get.CAR2.get.VALEUR.get
    val query = URLEncoder.encode(reportElfin.DIVERS.get.METHODE.get, "UTF-8")

    
    val responseFuture = XQueryWSHelper.runWrappedXQueryFile(query.trim, None)

    responseFuture.map { response =>
      val resultData = XML.loadString(response.body)
      val tempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportHeader", ".html"))
      play.api.libs.Files.writeFile(tempFile file, renderTemplate(headerTemplateName, resultData))

      val page = renderTemplate(contentTemplateName, resultData)

      val pdf = Pdf(
        reportConfig.wkhtmltopdfPath,
        new PdfConfig {
          orientation := Portrait
          pageSize := "A4"
          headerHtml := tempFile.file.getAbsolutePath

        }
      )

      val tempResult = new TemporaryFile(java.io.File.createTempFile("hb5Report", ".pdf"))

      pdf.run(page, tempResult.file)
      tempResult
    }
  }

  def toFormattedNumber(text:String):String = {

    try {
      val formatter = new DecimalFormat("###,###.##")
      val value = java.lang.Double.valueOf(text)
      formatter.format(value)
    } catch {
      case _:Throwable => ""

    }

  }


  def encodeImage(path:String, mimeType:String): String = {
    val isOption:Option[InputStream] = Play.current.resourceAsStream(path)

    isOption match {
      case Some(is) =>
        val bytes = new Array[Byte](is.available())
        is.read(bytes)
        val base64:Base64 = new Base64()
        // data:image/gif;base64,
        "data:" + mimeType + ";base64," + base64.encodeAsString(bytes)

      case None =>
        ""
    }
  }

}

