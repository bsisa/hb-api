package ch.bsisa.hyperbird.report

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.ws.{ WSQueries, XQueryWSHelper }
import ch.bsisa.hyperbird.model.ELFIN
import io.github.cloudify.scala.spdf.{ Landscape, PageOrientation, Portrait, PdfConfig, Pdf }
import org.apache.commons.codec.binary.Base64
import play.api.Logger
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
import java.io.File

/**
 * PDF report builder based on wkhtmltopdf
 */
object ReportBuilder {

  val REPORT_TITLE_QUERY_PARAM_NAME = "reportTitle"
  val REPORT_FILENAME_PREFIX_QUERY_PARAM_NAME = "reportFileNamePrefix"

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
   * <ELFIN Id="xxx" ID_G="xxx" CLASSE="RAPPORT" GROUPE="" TYPE="ACTIVITE" NATURE="Flux">
   * (...)
   * <CARACTERISTIQUE>
   * <CAR1 NOM="header" UNITE="reference" VALEUR="views.html.reports.defaultHeader"/>
   * <CAR2 NOM="content" UNITE="reference" VALEUR="views.html.reports.defaultBody"/>
   * <CAR3 NOM="footer" UNITE="reference" VALEUR="views.html.reports.defaultFooter"/>
   * <CAR4 NOM="query" UNITE="reference" VALEUR="myReportXQuery.xq"/>
   * <CAR5 NOM="filename" UNITE="name" VALEUR="FriendlyFileNamePrefix"/>
   * <CAR6 NOM="reportTitle" UNITE="name" VALEUR="The Report Title"/>
   * <CALCUL/>
   * </CARACTERISTIQUE>
   * (...)
   * </ELFIN>
   *
   */
  def writeReport(reportElfin: ELFIN, queryString: Option[String], queryStringMapOption: Option[Map[String, String]] = None)(implicit reportConfig: ReportConfig, apiConfig: ch.bsisa.hyperbird.ApiConfig): Future[TemporaryFile] = {

    // ==============================================================
    // Extract parameters from reportElfin
    // ==============================================================    
    val headerTemplateNameCar1Option = reportElfin.CARACTERISTIQUE.get.CAR1
    val contentTemplateName = reportElfin.CARACTERISTIQUE.get.CAR2.get.VALEUR.get
    val footerTemplateName = reportElfin.CARACTERISTIQUE.get.CAR3.get.VALEUR.get
    val queryFileName = reportElfin.CARACTERISTIQUE.get.CAR4.get.VALEUR.get
    // Extract CARSET.CAR[@NAME='headerMessage']/@VALEUR and return the configured message as string
//    val headerMessageOption: Option[String] = reportElfin.CARACTERISTIQUE.get.CARSET match {
//        case Some(carset) =>
//          carset.CAR.find(car => car.NOM.getOrElse(false) == "headerMessage") match {
//            case Some(car) => car.VALEUR
//            case None => None
//          }
//        case None => None
//      }
    
    val headerMessageOption: Option[String] = reportElfin.CARACTERISTIQUE.get.CARSET.flatMap{ carset =>
      carset.CAR.find{ car => car.NOM.getOrElse(false) == "headerMessage" }.flatMap { car => car.VALEUR }
    }        

    // Get reportTitle, reportFileNamePrefix information from query parameters if available 
    // fallback to default from ELFIN report configuration otherwise.
    // This allow a single complex XQuery to produce reports with different names and titles
    // avoiding XQuery logic duplication 
    val reportParams = queryStringMapOption match {
      case Some(queryStringMap) =>
        Logger.debug(s"ReportBuilder.writeReport: queryStringMap = ${queryStringMap}")
        val reportFileNamePrefix = queryStringMap.get(REPORT_FILENAME_PREFIX_QUERY_PARAM_NAME) match {
          case Some(reportFileNamePrefix) => reportFileNamePrefix
          case None => reportElfin.CARACTERISTIQUE.get.CAR5.get.VALEUR.get
        }
        val reportTitle = queryStringMap.get(REPORT_TITLE_QUERY_PARAM_NAME) match {
          case Some(reportTitle) => reportTitle
          case None => reportElfin.CARACTERISTIQUE.get.CAR6.get.VALEUR.get
        }
        (reportTitle, reportFileNamePrefix)
      case None =>
        Logger.debug(s"ReportBuilder.writeReport: queryStringMap = None")
        val reportFileNamePrefix = reportElfin.CARACTERISTIQUE.get.CAR5.get.VALEUR.get
        val reportTitle = reportElfin.CARACTERISTIQUE.get.CAR6.get.VALEUR.get
        (reportTitle, reportFileNamePrefix)
    }

    // URL encoding is necessary for query content but not for file name
    //val queryFileName = URLEncoder.encode(reportElfin.DIVERS.get.METHODE.get, "UTF-8")

    // ==============================================================
    // Run XQuery by file name
    // ==============================================================
    
    // Add annexesRootFolderPath information if available in apiConfig
    // Note: First introduced to add print support to Gespatri IMMEUBLE latests 'photo' from ANNEXE
    val augmentedQueryString = if (apiConfig.annexesRootFolder.trim().size > 0) {
        val annexesRootFolderPath = apiConfig.annexesRootFolder
        val queryStringWithAnnexesRootFolderPath = queryString match {
          case Some(qs) => qs + s"&annexesRootFolderPath=${annexesRootFolderPath}"
          case None => s"?annexesRootFolderPath=${annexesRootFolderPath}"
        }       
        Some(queryStringWithAnnexesRootFolderPath)
    } else {
      queryString
    }
    
    val responseFuture = XQueryWSHelper.runXQueryFile(queryFileName.trim, augmentedQueryString)

    responseFuture.map { response =>

      val reportTitle = reportParams._1
      val reportFileNamePrefix = reportParams._2

      // Work with String expected to contain HTML.
      val resultData = response.body
      // XML data unused at the moment.
      //val resultData = XML.loadString(respBody)

      // Extract CARSET.CAR[@NAME='pageOrientation']/@VALEUR and return whether orientation is portrait or landscape
      val pageOrientation: PageOrientation = reportElfin.CARACTERISTIQUE.get.CARSET match {
        case Some(carset) =>
          carset.CAR.find(car => car.NOM.getOrElse(false) == "pageOrientation") match {
            case Some(car) => if (car.VALEUR.getOrElse("not-defined") == "landscape") Landscape else Portrait
            case None => Portrait
          }
        case None => Portrait
      }      
      
      // Render report footer to HTML and save it to disk
      val reportFooterHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportFooter", ".html"))
      play.api.libs.Files.writeFile(reportFooterHtmlTempFile.file, renderTemplate(footerTemplateName, resultData, reportTitle))

      // Render report body to HTML and save it to disk
      val reportContentHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportContent", ".html"))
      
      //val reportContentHtmlString = renderTemplate(contentTemplateName, resultData, reportTitle)
      play.api.libs.Files.writeFile(reportContentHtmlTempFile.file, renderTemplate(contentTemplateName, resultData, reportTitle))


      // If defined, render report header to HTML and save it to disk
      val pdf = headerTemplateNameCar1Option match {
        case Some(htn) =>       
          // Render report header to HTML and save it to disk
          val reportHeaderHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportHeader", ".html"))
          // If defined, pass header message to template
          headerMessageOption match {
            case Some(headerMessage) => 
              play.api.libs.Files.writeFile(reportHeaderHtmlTempFile.file, renderTemplate(htn.VALEUR.get, headerMessage, reportTitle))
              // Configure wkhtmltopdf with header
              Pdf(
                reportConfig.wkhtmltopdfPath,
                new PdfConfig {
                  orientation := pageOrientation
                  pageSize := "A4"
                  headerHtml := reportHeaderHtmlTempFile.file.getAbsolutePath
                  footerHtml := reportFooterHtmlTempFile.file.getAbsolutePath
                }
              )  
            case None =>
              play.api.libs.Files.writeFile(reportHeaderHtmlTempFile.file, renderTemplate(htn.VALEUR.get, resultData, reportTitle))
              // Configure wkhtmltopdf with header
              Pdf(
                reportConfig.wkhtmltopdfPath,
                new PdfConfig {
                  orientation := pageOrientation
                  pageSize := "A4"
                  headerHtml := reportHeaderHtmlTempFile.file.getAbsolutePath
                  footerHtml := reportFooterHtmlTempFile.file.getAbsolutePath
                }
              )  
            }
          
          
        
        case None => 
          // Configure wkhtmltopdf without header (useful to gain print space for drawing or stickers specific configurations)
          Pdf(
              reportConfig.wkhtmltopdfPath,
              new PdfConfig {
                orientation := pageOrientation
                pageSize := "A4"
                footerHtml := reportFooterHtmlTempFile.file.getAbsolutePath
              }
          )        
      }
        
      // Create empty temporary file for final PDF report outcome.
      val tempResult = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))
      // Process HTML temporary files to PDF using wkhtmltopdf
      pdf.run(reportContentHtmlTempFile.file, tempResult.file)

      tempResult
    }
  }

  /**
   * Reads a binary file reachable at `path` in `Play.current.resourceAsStream` context as base64 String with specified `mimeType`
   */
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

  /**
   * Reads a file reachable at `path` in `Play.current.resourceAsStream` context as String.
   */
  def readFileToString(path: String): String = {
    val isOption: Option[InputStream] = Play.current.resourceAsStream(path)
    isOption match {
      case Some(is) =>
        val jsString = scala.io.Source.fromInputStream(is).getLines mkString "\n"
        is.close()
        jsString
      case None => ""
    }
  }

}

