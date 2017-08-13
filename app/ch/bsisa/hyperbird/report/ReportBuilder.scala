package ch.bsisa.hyperbird.report

import java.io.{FileWriter, InputStream}

import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.report.dao.ReportDAO
import ch.bsisa.hyperbird.util.ElfinUtil
import io.github.cloudify.scala.spdf.{Landscape, Pdf, PdfConfig, Portrait}
import org.apache.commons.codec.binary.Base64
import play.api.{Logger, Play}
import play.api.libs.Files.TemporaryFile
import play.api.mvc.Result
import play.api.templates.{Template1, Template2, Template3}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

/**
 * PDF report builder based on wkhtmltopdf
 */
object ReportBuilder {

  val REPORT_TITLE_QUERY_PARAM_NAME = "reportTitle"
  val REPORT_FILENAME_PREFIX_QUERY_PARAM_NAME = "reportFileNamePrefix"
  val REPORT_HEADER_DYNAMIC_PARAMETER_1_QUERY_PARAM_NAME = "reportHeaderParam1"
  val REPORT_HEADER_DYNAMIC_PARAMETER_2_QUERY_PARAM_NAME = "reportHeaderParam2"
  val REPORT_CALLER_ELFIN_ID = "id"
  val REPORT_CALLER_ELFIN_ID_G = "col"

  /**
   * Renders template with three parameters by name to String for wkhtmltopdf to process
   */
  def renderTemplate[A](templateName: String, data: A, reportTitle: String, headerMessage: String): String = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val template = m.reflectModule(m.staticModule(templateName + "$")).instance.asInstanceOf[Template3[A, String, String, Result]]
    template.render(data, reportTitle, headerMessage).toString
  }

  /**
   * Renders template with two parameters by name to String for wkhtmltopdf to process
   */
  def renderTemplate[A](templateName: String, data: A, reportTitle: String): String = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val template = m.reflectModule(m.staticModule(templateName + "$")).instance.asInstanceOf[Template2[A, String, Result]]
    template.render(data, reportTitle).toString
  }

  /**
   * Renders template with a single parameter by name to String for wkhtmltopdf to process
   */
  def renderTemplate(templateName: String, reportTitle: String): String = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val template = m.reflectModule(m.staticModule(templateName + "$")).instance.asInstanceOf[Template1[String, Result]]
    template.render(reportTitle).toString
  }

  /**
    * Resolves the report parameters
    * @param reportElfin
    * @param queryStringMapOption
    * @return
    */

  def getReportParameters(reportElfin: ELFIN, queryStringMapOption: Option[Map[String, String]] = None):(String, String) = queryStringMapOption match {
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

  /**
    * Concatenates the ANNEXES documents without the main report
    * This method shares a lot with thwe writeReport method.
    * TODO: Try to factorize both methods
    */
  def writeAnnexeReport(reportElfin: ELFIN, queryString: Option[String], queryStringMapOption: Option[Map[String, String]] = None)
                       (implicit reportConfig: ReportConfig, apiConfig: ch.bsisa.hyperbird.ApiConfig): TemporaryFile = {
    val reportParams = getReportParameters( reportElfin, queryStringMapOption)

    // Optional identity of the ELFIN data source calling the report
    val callerId = queryStringMapOption.flatMap(_.get(REPORT_CALLER_ELFIN_ID))
    val callerID_G = queryStringMapOption.flatMap(_.get(REPORT_CALLER_ELFIN_ID_G))

    val reportFileNamePrefix = reportParams._2


    // Easily achieved
    // using the Option nature of dyn res.
    // Perform dynamic merging if caller Id, ID_G are available.
    val reportWithDynTempResultOpt = if (callerId.isDefined && callerID_G.isDefined) {
      Logger.debug(s">>>> CALLER: ID_G/Id = ${callerId}/${callerID_G}" );
      val futureFilepathsOptToMerge = ReportDAO.getPdfAnnexPathsToMerge(elfinId = callerId.get, elfinID_G = callerID_G.get)
      val res =
        futureFilepathsOptToMerge map { _.map { filepathsToMerge =>
          val filepathsToMergeBefore = filepathsToMerge._1
          val filepathsToMergeAfter = filepathsToMerge._2

          // Merge files
          val inputFilesAbsPathNameList = filepathsToMergeBefore ++ filepathsToMergeAfter
          // Create empty temporary file for PDF report content including dynamic outcome.
          val reportWithDynTempResult = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))
          val mergeExitCode = PdfFileMergingHelper.mergePdfFiles(inputFilesAbsPathNameList, reportWithDynTempResult.file.getCanonicalPath)
          if (mergeExitCode != 0) {
            Logger.error(s"ReportBuilder.writeReport: Failure while merging PDF file: mergePdfFiles exit code = ${mergeExitCode}")
          }
          Logger.debug(s">>>> reportWithDynTempResult at ${reportWithDynTempResult.file.getCanonicalPath}")
          reportWithDynTempResult
        }
        }
      import scala.concurrent.duration._
      val tempRes = Await.result(res, 1 minutes)
      tempRes
    } else {
      None
    }

    // Perform static merging if configuration is available and referred document contains the expected PDF annex to merge with.

    // Extract optional 'pdfIncludeLast' configuration as Option[String]
    val pdfIncludeLastTripletIdentifierOption = ElfinUtil.getElfinCarByName(reportElfin, ReportConfig.CAR_NAME_PDF_INCLUDE_LAST).flatMap(_.VALEUR)


    // Create empty temporary file for PDF report content outcome.
    val emptyFile = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))
    val writer = new FileWriter(emptyFile.file)
    writer.write("%PDF 1 0 obj<</Pages<</Kids[<</Contents<<>>stream\nBT 9 Tf(PAS DE CONTENU)' ET endstream>>]>>>>endobj trailer<</Root 1 0 R>>")
    writer.close()

    // Proceed with merge if necessary document and annex are available
    val mergedWithPdfIncludeLastFileOpt = pdfIncludeLastTripletIdentifierOption flatMap { triplet =>
      val futureTmpRes = ReportDAO.getFirstPdfAnnexe(triplet) map { fileOpt =>
        fileOpt.map { file =>
          // Merge file at end
          val inputFilesAbsPathNameList =
            reportWithDynTempResultOpt match {
              case Some(reportWithDynTempResult) =>  Seq(reportWithDynTempResult.file.getCanonicalPath, file.getCanonicalPath)
              case None => Seq(emptyFile.file.getCanonicalPath, file.getCanonicalPath)
            }
          val tempMergedResult = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))
          val mergeExitCode = PdfFileMergingHelper.mergePdfFiles(inputFilesAbsPathNameList, tempMergedResult.file.getCanonicalPath)
          if (mergeExitCode != 0) {
            Logger.error(s"ReportBuilder.writeReport: Failure while merging PDF file: mergePdfFiles exit code = ${mergeExitCode}")
          }
          tempMergedResult
        }
      }
      import scala.concurrent.duration._
      val tempRes = Await.result(futureTmpRes, 1 minutes)
      tempRes
    }


    // Extract optional 'pdfIncludeFirst' configuration as Option[String]
    val pdfIncludeFirstTripletIdentifierOption = ElfinUtil.getElfinCarByName(reportElfin, ReportConfig.CAR_NAME_PDF_INCLUDE_FIRST).flatMap(_.VALEUR)

    // Proceed with merge if necessary document and annex are available
    val mergedWithPdfIncludeFirstFileOpt = pdfIncludeFirstTripletIdentifierOption flatMap { triplet =>
      val futureTmpRes = ReportDAO.getFirstPdfAnnexe(triplet) map { fileOpt =>
        fileOpt.map { file =>
          // Merge file at beginning
          val inputFilesAbsPathNameList =
            mergedWithPdfIncludeLastFileOpt match {
              case Some(mergedWithPdfIncludeLastFile) => Seq(file.getCanonicalPath, mergedWithPdfIncludeLastFile.file.getCanonicalPath)
              case None => {
                reportWithDynTempResultOpt match {
                  case Some(reportWithDynTempResult) =>  Seq(file.getCanonicalPath, reportWithDynTempResult.file.getCanonicalPath)
                  case None => Seq(file.getCanonicalPath, emptyFile.file.getCanonicalPath)
                }
              }
            }
          val tempMergedResult = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))
          val mergeExitCode = PdfFileMergingHelper.mergePdfFiles(inputFilesAbsPathNameList, tempMergedResult.file.getCanonicalPath)
          if (mergeExitCode != 0) {
            Logger.error(s"ReportBuilder.writeReport: Failure while merging PDF file: mergePdfFiles exit code = ${mergeExitCode}")
          }
          tempMergedResult
        }
      }
      import scala.concurrent.duration._
      val tempRes = Await.result(futureTmpRes, 1 minutes)
      tempRes
    }

    // Return merged documents if applicable and available otherwise return first generated PDF unchanged
    mergedWithPdfIncludeFirstFileOpt match {
      case Some(mergedWithPdfIncludeFirstFile) => mergedWithPdfIncludeFirstFile // Built last it contains report content, dynamic includes and both static includes depending on their availability.
      case None => {
        mergedWithPdfIncludeLastFileOpt match {
          case Some(mergedWithPdfIncludeLastFile) => mergedWithPdfIncludeLastFile
          case None => {
            reportWithDynTempResultOpt match {
              case Some(reportWithDynTempResult) => reportWithDynTempResult
              case None => emptyFile
            }
          }
        }
      }
    }
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

    // Manage optional header parameters list
    // Optional static
    // Extract optional CARSET.CAR[@NAME='headerMessage']/@VALEUR configured message as Option[String]
    val staticHeaderMessageOption = ElfinUtil.getElfinCarByName(reportElfin, ReportConfig.CAR_NAME_HEADER_MESSAGE).flatMap(_.VALEUR)
    // Optional dynamic
    val headerDynParam1 = queryStringMapOption.flatMap(_.get(REPORT_HEADER_DYNAMIC_PARAMETER_1_QUERY_PARAM_NAME))
    val headerDynParam2 = queryStringMapOption.flatMap(_.get(REPORT_HEADER_DYNAMIC_PARAMETER_2_QUERY_PARAM_NAME))

    // This sequence of parameters will be empty if no optional parameters were defined 
    val headerParams = Seq(staticHeaderMessageOption,headerDynParam1,headerDynParam2).filter( _.isDefined ).map( _.get )

    // Get reportTitle, reportFileNamePrefix information from query parameters if available 
    // fallback to default from ELFIN report configuration otherwise.
    // This allow a single complex XQuery to produce reports with different names and titles
    // avoiding XQuery logic duplication 
    val reportParams = getReportParameters( reportElfin, queryStringMapOption)

    // Optional identity of the ELFIN data source calling the report
    val callerId = queryStringMapOption.flatMap(_.get(REPORT_CALLER_ELFIN_ID))
    val callerID_G = queryStringMapOption.flatMap(_.get(REPORT_CALLER_ELFIN_ID_G))

    // URL encoding is necessary for query content but not for file name
    //val queryFileName = URLEncoder.encode(reportElfin.DIVERS.get.METHODE.get, "UTF-8")


    // ==============================================================
    // Prepare `Augmented` Query string 
    // ==============================================================

    // Additional parameter values
    val annexesRootFolderPathOpt = if (apiConfig.annexesRootFolder.trim().size > 0) Some(apiConfig.annexesRootFolder) else None
    val reportWaterMarkElfinRefOpt = ElfinUtil.getElfinCarByName( reportElfin, ReportConfig.CAR_NAME_WATERMARK_ELFIN_REF ).flatMap( _.VALEUR )

    // Additional parameter key to value map
    val optionalParamsToQuery = Map(
    "annexesRootFolderPath" -> annexesRootFolderPathOpt,
    ReportConfig.CAR_NAME_WATERMARK_ELFIN_REF -> reportWaterMarkElfinRefOpt
    )

//    // Remove any key -> value where no value has been provided to
//    val paramsToQuery = optionalParamsToQuery.filter( { case(key,value) => value.isDefined } ).map( { case(key,value) => (key,value.get) } );
//
//    // Create augmentedQueryString merging additional parameters to
//    // existing query string as necessary
//    // Remark: parameters come from controlled configurations and do not require encoding.
//    val augmentedQueryString =
//      // Additional parameters are available
//      if (paramsToQuery.size > 0) {
//        val queryStringWithNewParams = queryString match {
//            // An existing query need to be `augmented`
//            case Some(qs) =>
//              val nextParams = for {
//                (k,v) <- paramsToQuery
//              } yield {
//                  s"&${k}=${v}"
//              }
//              val finalQueryString = qs + nextParams.mkString
//              finalQueryString
//            // No existing query, build one.
//            case None =>
//              val params = for {
//                ((k,v),i) <- paramsToQuery.view.zipWithIndex
//              } yield {
//                val paramSep = if (i!=0) {"&"} else {"?"}
//                paramSep+s"${k}=${v}"
//              }
//              params.mkString
//          }
//        Some(queryStringWithNewParams)
//    } else { // No additional parameters are available, return queryString unchanged.
//      queryString
//    }


    val augmentedQueryString = buildOrAugmentQueryString( queryString , optionalParamsToQuery )


    // ==============================================================
    // Run XQuery by file name
    // ==============================================================    

    val responseFuture = XQueryWSHelper.runXQueryFile(queryFileName.trim, augmentedQueryString)

    responseFuture.map { response =>

      val reportTitle = reportParams._1
      val reportFileNamePrefix = reportParams._2

      // Work with String expected to contain HTML.
      val resultData = response.body

      // XML data unused at the moment.
      //val resultData = XML.loadString(respBody)

      // Extract CARSET.CAR[@NAME='pageOrientation']/@VALEUR and return whether orientation is portrait or landscape
      val pageOrientationValueOption = ElfinUtil.getElfinCarByName( reportElfin, ReportConfig.CAR_NAME_PAGE_ORIENTATION ).flatMap( _.VALEUR )
      val pageOrientation = pageOrientationValueOption match {
        case Some(pageOrientationValue) => if (pageOrientationValue == ReportConfig.CAR_VALUE_PAGE_ORIENTATION_LANDSCAPE) Landscape else Portrait
        case None => Portrait
      }

      // Render report footer to HTML and save it to disk
      val reportFooterHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportFooter", ".html"))
      play.api.libs.Files.writeFile(reportFooterHtmlTempFile.file, renderTemplate(footerTemplateName, reportTitle))

      // Render report body to HTML and save it to disk
      val reportContentHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportContent", ".html"))
      play.api.libs.Files.writeFile(reportContentHtmlTempFile.file, renderTemplate(contentTemplateName, resultData, reportTitle))


      // If defined, render report header to HTML and save it to disk
      val pdfOpt : Option[Pdf] = headerTemplateNameCar1Option match {
        case Some(htn) =>
          // Render report header to HTML and save it to disk
          val reportHeaderHtmlTempFile = new TemporaryFile(java.io.File.createTempFile("hb5ReportHeader", ".html"))
          // If any report header parameters defined pass them to template
          if ( headerParams.size > 0 ) {
            if ( headerParams.size == 1 ) {
              play.api.libs.Files.writeFile(reportHeaderHtmlTempFile.file, renderTemplate(htn.VALEUR.get, headerParams.head, reportTitle))
              // Configure wkhtmltopdf with header
              Some(Pdf(
                reportConfig.wkhtmltopdfPath,
                new PdfConfig {
                  orientation := pageOrientation
                  pageSize := "A4"
                  headerHtml := reportHeaderHtmlTempFile.file.getAbsolutePath
                  footerHtml := reportFooterHtmlTempFile.file.getAbsolutePath
                }
              ))
            } else if ( headerParams.size == 2) {
              play.api.libs.Files.writeFile(reportHeaderHtmlTempFile.file, renderTemplate(htn.VALEUR.get, headerParams.head, headerParams.tail.head, reportTitle))
              // Configure wkhtmltopdf with header
              Some(Pdf(
                reportConfig.wkhtmltopdfPath,
                new PdfConfig {
                  orientation := pageOrientation
                  pageSize := "A4"
                  headerHtml := reportHeaderHtmlTempFile.file.getAbsolutePath
                  footerHtml := reportFooterHtmlTempFile.file.getAbsolutePath
                }
              ))
            } else {
              // Configuration with headerMessages size greater than 2 not yet supported
              Logger.error(s"ReportBuilder.writeReport: Configuration with headerParams size ${headerParams.size} greater than 2 not yet supported!")
              None
            }
          } else {
            play.api.libs.Files.writeFile(reportHeaderHtmlTempFile.file, renderTemplate(htn.VALEUR.get, reportTitle))
            // Configure wkhtmltopdf with header
            Some(Pdf(
              reportConfig.wkhtmltopdfPath,
              new PdfConfig {
                orientation := pageOrientation
                pageSize := "A4"
                headerHtml := reportHeaderHtmlTempFile.file.getAbsolutePath
                footerHtml := reportFooterHtmlTempFile.file.getAbsolutePath
              }
            ))
          }
        case None =>
          // Configure wkhtmltopdf without header (useful to gain print space for drawing or stickers specific configurations)
          Some(Pdf(
              reportConfig.wkhtmltopdfPath,
              new PdfConfig {
                orientation := pageOrientation
                pageSize := "A4"
                footerHtml := reportFooterHtmlTempFile.file.getAbsolutePath
              }
          ))
      }

      // Create empty temporary file for PDF report content outcome.
      val reportContentTempResult = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))

      pdfOpt match {
        case Some(pdf) =>
          // Process HTML temporary files to PDF using wkhtmltopdf
          val exitCode = pdf.run(reportContentHtmlTempFile.file, reportContentTempResult.file)

          // If PDF creation succeeded
          if (exitCode == 0) {

            // TODO : Adapt workflow to merge dynamic and static and return the expected result
            // Easily achieved using the Option nature of dyn res.
            // Perform dynamic merging if caller Id, ID_G are available.
            val reportWithDynTempResultOpt = if (callerId.isDefined && callerID_G.isDefined) {
              Logger.debug(s">>>> CALLER: ID_G/Id = ${callerId}/${callerID_G}" );
              val futureFilepathsOptToMerge = ReportDAO.getPdfAnnexPathsToMerge(elfinId = callerId.get, elfinID_G = callerID_G.get)
              val res =
                futureFilepathsOptToMerge map { _.map { filepathsToMerge =>
                  val filepathsToMergeBefore = filepathsToMerge._1
                  val filepathsToMergeAfter = filepathsToMerge._2

                  // Merge files 
                  val inputFilesAbsPathNameList = filepathsToMergeBefore ++ Seq(reportContentTempResult.file.getCanonicalPath) ++ filepathsToMergeAfter
                  // Create empty temporary file for PDF report content including dynamic outcome.
                  val reportWithDynTempResult = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))
                  val mergeExitCode = PdfFileMergingHelper.mergePdfFiles(inputFilesAbsPathNameList, reportWithDynTempResult.file.getCanonicalPath)
                  if (mergeExitCode != 0) {
                    Logger.error(s"ReportBuilder.writeReport: Failure while merging PDF file: mergePdfFiles exit code = ${mergeExitCode}")
                  }
                  Logger.debug(s">>>> reportWithDynTempResult at ${reportWithDynTempResult.file.getCanonicalPath}")
                  reportWithDynTempResult
                }
              }
              import scala.concurrent.duration._
              val tempRes = Await.result(res, 1 minutes)
              tempRes
            } else {
              None
            }


            // Perform static merging if configuration is available and referred document contains the expected PDF annex to merge with.

            // Extract optional 'pdfIncludeLast' configuration as Option[String]
            val pdfIncludeLastTripletIdentifierOption = ElfinUtil.getElfinCarByName(reportElfin, ReportConfig.CAR_NAME_PDF_INCLUDE_LAST).flatMap(_.VALEUR)

            // Proceed with merge if necessary document and annex are available
            val mergedWithPdfIncludeLastFileOpt = pdfIncludeLastTripletIdentifierOption flatMap { triplet =>
              val futureTmpRes = ReportDAO.getFirstPdfAnnexe(triplet) map { fileOpt =>
                fileOpt.map { file =>
                  // Merge file at end
                  val inputFilesAbsPathNameList =
                   reportWithDynTempResultOpt match {
                    case Some(reportWithDynTempResult) =>  Seq(reportWithDynTempResult.file.getCanonicalPath, file.getCanonicalPath)
                    case None => Seq(reportContentTempResult.file.getCanonicalPath, file.getCanonicalPath)
                  }
                  val tempMergedResult = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))
                  val mergeExitCode = PdfFileMergingHelper.mergePdfFiles(inputFilesAbsPathNameList, tempMergedResult.file.getCanonicalPath)
                  if (mergeExitCode != 0) {
                    Logger.error(s"ReportBuilder.writeReport: Failure while merging PDF file: mergePdfFiles exit code = ${mergeExitCode}")
                  }
                  tempMergedResult
                }
              }
              import scala.concurrent.duration._
              val tempRes = Await.result(futureTmpRes, 1 minutes)
              tempRes
            }


            // Extract optional 'pdfIncludeFirst' configuration as Option[String]
            val pdfIncludeFirstTripletIdentifierOption = ElfinUtil.getElfinCarByName(reportElfin, ReportConfig.CAR_NAME_PDF_INCLUDE_FIRST).flatMap(_.VALEUR)

            // Proceed with merge if necessary document and annex are available
            val mergedWithPdfIncludeFirstFileOpt = pdfIncludeFirstTripletIdentifierOption flatMap { triplet =>
              val futureTmpRes = ReportDAO.getFirstPdfAnnexe(triplet) map { fileOpt =>
                fileOpt.map { file =>
                  // Merge file at beginning
                  val inputFilesAbsPathNameList =
                    mergedWithPdfIncludeLastFileOpt match {
                    case Some(mergedWithPdfIncludeLastFile) => Seq(file.getCanonicalPath, mergedWithPdfIncludeLastFile.file.getCanonicalPath)
                    case None => {
                      reportWithDynTempResultOpt match {
                        case Some(reportWithDynTempResult) =>  Seq(file.getCanonicalPath, reportWithDynTempResult.file.getCanonicalPath)
                        case None => Seq(file.getCanonicalPath, reportContentTempResult.file.getCanonicalPath)
                      }
                    }
                  }
                  val tempMergedResult = new TemporaryFile(java.io.File.createTempFile(reportFileNamePrefix, ".pdf"))
                  val mergeExitCode = PdfFileMergingHelper.mergePdfFiles(inputFilesAbsPathNameList, tempMergedResult.file.getCanonicalPath)
                  if (mergeExitCode != 0) {
                    Logger.error(s"ReportBuilder.writeReport: Failure while merging PDF file: mergePdfFiles exit code = ${mergeExitCode}")
                  }
                  tempMergedResult
                }
              }
              import scala.concurrent.duration._
              val tempRes = Await.result(futureTmpRes, 1 minutes)
              tempRes
            }

            // Return merged documents if applicable and available otherwise return first generated PDF unchanged
            mergedWithPdfIncludeFirstFileOpt match {
              case Some(mergedWithPdfIncludeFirstFile) => mergedWithPdfIncludeFirstFile // Built last it contains report content, dynamic includes and both static includes depending on their availability.
              case None => {
                mergedWithPdfIncludeLastFileOpt match {
                  case Some(mergedWithPdfIncludeLastFile) => mergedWithPdfIncludeLastFile
                  case None => {
                    reportWithDynTempResultOpt match {
                        case Some(reportWithDynTempResult) => reportWithDynTempResult
                        case None => reportContentTempResult
                    }
                  }
                }
              }
            }
          } else {
            // If exitCode for PDF generation is not equal to zero it failed. Do not perform any extra process.
            Logger.error(s"ReportBuilder.writeReport: Failure while generating PDF file: pdf.run exit code = ${exitCode}")
            reportContentTempResult
          }
        case None =>
            // If exitCode for PDF generation is not equal to zero it failed. Do not perform any extra process.
            Logger.error(s"ReportBuilder.writeReport: Obtained invalid number of header parameters. Should be 0 to 2 maximum.")
            reportContentTempResult
      }


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


  /**
   * Builds a new query string from `parameterValueMap` if queryString is empty.
   * Returns `queryString` unchanged if parameterValueMap is empty.
   * Returns `queryString` merged with `parameterValueMap` if both contain values.
   * Returns None if neither `queryString` nor `parameterValueMap` contain value.
   */
  def buildOrAugmentQueryString( queryString : Option[String], parameterValueMap : Map[String,Option[String]] ) : Option[String] = {

    // Remove any key -> value where no value has been provided to
    val paramsToQuery = parameterValueMap.filter( { case(key,value) => value.isDefined } ).map( { case(key,value) => (key,value.get) } );

    // Create augmentedQueryString merging additional parameters to 
    // existing query string as necessary
    // Remark: parameters come from controlled configurations and do not require encoding.
    val augmentedQueryString =
      // Additional parameters are available
      if (paramsToQuery.size > 0) {
        val queryStringWithNewParams = queryString match {
            // An existing query need to be `augmented`
            case Some(qs) =>
              val nextParams = for {
                (k,v) <- paramsToQuery
              } yield {
                  s"&${k}=${v}"
              }
              val finalQueryString = qs + nextParams.mkString
              finalQueryString
            // No existing query, build one.
            case None =>
              val params = for {
                ((k,v),i) <- paramsToQuery.view.zipWithIndex
              } yield {
                val paramSep = if (i!=0) {"&"} else {"?"}
                paramSep+s"${k}=${v}"
              }
              params.mkString
          }
        Some(queryStringWithNewParams)
    } else { // No additional parameters are available, return queryString unchanged.
      queryString
    }
    augmentedQueryString
  }


}

