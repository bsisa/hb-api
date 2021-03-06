package ch.bsisa.hyperbird.controllers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.net.ConnectException
import java.util.Date

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.{CollectionsConfig, InitConfig}
import ch.bsisa.hyperbird.cache.CacheHelper
import ch.bsisa.hyperbird.dao.{ElfinDAO, ResultNotFoundException}
import ch.bsisa.hyperbird.dao.ws.{WSQueries, XQueryWSHelper}
import ch.bsisa.hyperbird.documents.{SpreadSheetBuilder, WordDocumentBuilder}
import ch.bsisa.hyperbird.io._
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.model.format.ElfinFormat.ElfinFormatException
import ch.bsisa.hyperbird.report.ReportBuilder
import ch.bsisa.hyperbird.security.social.HbSecureService.PasswordHashException
import ch.bsisa.hyperbird.security.social._
import ch.bsisa.hyperbird.util.{ElfinUtil, FunctionsUtil}
import org.apache.poi.ss.usermodel._
import org.apache.poi.xwpf.usermodel.XWPFDocument
import play.api.Logger
import play.api.libs.Files
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._

import scala.concurrent.{Await, Future}

/**
 * REST API controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 * @author Guy de Pourtales
 */
object Api extends Controller with securesocial.core.SecureSocial {

  // Compressed JSON format
  val JSON_FORMAT = "json"
  // Pretty formatted JSON format
  val JSON_PRETTY_FORMAT = "json-pretty"
  // XML format
  val XML_FORMAT = "xml"
  // Database stored original format: can be any of XML, XQuery, PDF (binary),... format
  val ORIGINAL_FORMAT = "original"

  /**
   * Helper function obtaining configuration information.
   */
  private def getConfigJson()(implicit initConfig: InitConfig, collectionsConfig: CollectionsConfig): String = {
    val configJson =
      s"""{"config": { 
    "hb_init_ref": { 
      "Id": "${initConfig.initElfinId}",
      "ID_G": "${collectionsConfig.configurationCollectionId}"
    }
  }
}"""
    configJson
  }

  /**
   * Triggers backup dedicated XQuery script only if called from local host.
   *
   * This allows local server scripts to trigger backup without requiring
   * authentication information. This assumes restricted server "localhost"
   * access.
   */
  def runBackup(): Action[AnyContent] = Action.async { request =>
    val LOCALHOST = "localhost"
    val hostTokens = request.host.split(":")
    val isLocalHost = hostTokens.nonEmpty && hostTokens(0) == LOCALHOST
    if (isLocalHost) {
      val xqueryFileName = "admin_backup.xq"
      Logger.debug(s"""Run XQuery $xqueryFileName from host $LOCALHOST with XML returned format such as <backup date="2015-01-06T14:12:59.249+01:00" />""")

      XQueryWSHelper.runXQueryFile(xqueryFileName, None).map { response =>
        Ok(response.body).as(response.ahcResponse.getContentType)
      }
    } else {
      val backupDateString = ch.bsisa.hyperbird.util.DateUtil.elfinIdentifiantDateFormat.format(new java.util.Date())
      val messageString = s"Security policy only allows backup maintenance operation from $LOCALHOST"
      scala.concurrent.Future(Forbidden(<backup date={ backupDateString } status="forbidden" message={ messageString }/>))
    }
  }

  /**
   * Allow clearing all cached entries with keys starting with `http`
   */
  def clearCache(): Action[AnyContent] = SecuredAction(ajaxCall = true) {
    CacheHelper.removeAllEntries
    val currentDate = new Date()
    val jsonMsg = s"""
		{
			"cache": {
		        "status": "cleared",
		        "time": "$currentDate"
		    }
		}
	  """
    Ok(jsonMsg).as(JSON)
  }

  /**
   * Dynamically provides initialisation configuration information useful to hb-ui
   */
  def config(): Action[AnyContent] = SecuredAction(ajaxCall = true) {
    Ok(getConfigJson).as(JSON)
  }

  /**
   * TODO: review specifications. Listing collections.
   * <ul>
   * <li>Do we really have any use case for this ?</li>
   * <li>What type of data to return in geoXml.xsd world ?</li>
   * </ul>
   */
  // Unused
  //  def collections = SecuredAction(ajaxCall = true).async {
  //    XQueryWSHelper.query(WSQueries.allHbCollectionsQuery)
  //  }

  /**
   * Provides password hashing service.
   */
  def getPasswordHash(plainTextPassword: String): Action[AnyContent] = SecuredAction(ajaxCall = true) { request =>
    //
    try {
      val plainTextPasswordHashValue = HbSecureService.getPasswordHash(plainTextPassword)
      val jsonResponse =
        s"""{
	      "hash" : "$plainTextPasswordHashValue"
}"""
      Ok(jsonResponse).as(JSON)
    } catch {
      case phe: PasswordHashException =>
        managePasswordHashException(exception = phe, errorMsg = Option(s"User: ${request.user} failed to obtain password hash."))
      case e: Throwable =>
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"User: ${request.user} failed to obtain password hash."))
    }

  }

  /**
   * Returns the result of executing the specified XQuery file by name.
   *
   * Supported `format` parameter value are `{original|json}`
   *
   * Warning: The API is ELFIN centered. Selecting json means the native query results in XML
   * format will be converted to ELFIN in JSON format.
   * If the XML returned is not an ELFIN format then use `original` format instead and proceed to whatever
   * conversion afterward.
   */
  def runXQueryFile(xqueryFileName: String, format: String): Action[AnyContent] = SecuredAction(ajaxCall = true).async { request =>
    val queryString = if (request.rawQueryString != null && request.rawQueryString.nonEmpty) Option(request.rawQueryString) else None
    Logger.debug(s"Run XQuery $xqueryFileName with returned format = $format and rawQueryString: $queryString")

    XQueryWSHelper.runXQueryFile(xqueryFileName, queryString).map { response =>
      format match {
        case JSON_FORMAT =>
          Logger.debug(s">>>> runXQueryFile: Result of type ${response.ahcResponse.getContentType} received")
          val melfinWrappedBody = "<MELFIN>" + response.body.mkString + "</MELFIN>"
          Ok(ElfinFormat.elfinsToJsonArray(ElfinFormat.elfinsFromXml(scala.xml.XML.loadString(melfinWrappedBody))))
        case ORIGINAL_FORMAT => Ok(response.body).as(response.ahcResponse.getContentType)
      }
    }
  }

  /**
   * Returns the annex file if found HTTP error codes otherwise.
   * <i>Intended to HTTP GET requests</i>
   */
  def getElfinAnnexFile(elfinID_G: String, elfinId: String, fileName: String): Action[AnyContent] = SecuredAction(ajaxCall = true) { _ =>
    Logger.debug(s"getElfinAnnexFile(elfinID_G = $elfinID_G, elfinId = $elfinId, fileName = $fileName) called.")
    try {
      Ok.sendFile(AnnexesManager.getElfinAnnexFile(elfinID_G, elfinId, fileName))
    } catch {
      case e: Exception => manageAnnexesManagerException(e)
    }
  }

  /**
   * <i>Intended to HTTP HEAD requests</i>, in which case it should return:
   * <ul>
   *   <li>regular HTTP 200 if `annex file resource exists`</li>
   *   <li>custom HTTP 701 as successful `annex file resource does not exist` response.</li>
   *  <ul>
   */
  def cĥeckForElfinAnnexFile(elfinID_G: String, elfinId: String, fileName: String): Action[AnyContent] = SecuredAction(ajaxCall = true) { _ =>
    Logger.debug(s"cĥeckForElfinAnnexFile(elfinID_G = $elfinID_G, elfinId = $elfinId, fileName = $fileName) called.")
    try {
      Ok.sendFile(AnnexesManager.getElfinAnnexFile(elfinID_G, elfinId, fileName))
    } catch {
      case _: AnnexesManagerFileNotFoundException =>
        val jsonMsg = Json.obj(
          "ERROR" -> "annex.file.not.found",
          "DESCRIPTION" -> "HEAD call.")
        Status(701)(jsonMsg) // 701 - Custom application special Ok for HEAD not found
      case e: Exception => manageAnnexesManagerException(e)
    }
  }

  /**
   * Creates annex file at correct location following file chunks uploads.
   *
   * Note: REST upload is not practical given current upload library constraints.
   * CollectionId, elfinId, fileName informations are provided as POST parameters.
   * Creates the annex file pointed at by `ELFIN/ANNEXE/RENVOI/@LIEN` data structure
   */
  //  def createElfinAnnexFile(elfinID_G: String, elfinId: String, fileName: String) = Action(parse.multipartFormData) { request =>
  def createElfinAnnexFile(): Action[MultipartFormData[Files.TemporaryFile]] = Action(parse.multipartFormData) { request =>

    try {

      val params = request.body.asFormUrlEncoded

      //for ( key <- params.keys) { Logger.debug(s"key = ${key}")}

      // flow.js upload information
      val flowIdentifier = params("flowIdentifier").seq.head
      val flowChunkNumber = params("flowChunkNumber").seq.head.toInt
      val flowTotalChunks = params("flowTotalChunks").seq.head.toInt
      val flowFilename = params("flowFilename").seq.head
      val flowTotalSize = params("flowTotalSize").seq.head.toInt
      val flowChunkSize = params("flowChunkSize").seq.head.toInt

      // HyperBird upload information
      val elfinID_G = params.get("elfinID_G") match {
        case Some(seq) => seq.head
        case None      => throw new Exception("elfinID_G mandatory information missing.")
      }
      val elfinId = params.get("elfinId") match {
        case Some(seq) => seq.head
        case None      => throw new Exception("elfinId mandatory information missing.")
      }

      //      Logger.debug(s"""
      //      elfinID_G = ${elfinID_G}
      //      elfinId = ${elfinId}
      //      flowIdentifier = ${flowIdentifier}
      //      flowChunkNumber = ${flowChunkNumber}
      //      flowTotalChunks = ${flowTotalChunks}
      //      flowFilename = ${flowFilename}
      //      flowTotalSize = ${flowTotalSize}
      //      flowChunkSize = ${flowChunkSize}
      //    		""")

      // TODO: make it a config	  
      val chunksDirectory = FileUploadHelper.getTemporaryFileUploadDirectory()
      val chunkDestinationFile = new File(chunksDirectory, s"$flowIdentifier-$flowChunkNumber")
      Logger.debug(s"""
  chunksDirectory = ${chunksDirectory.getCanonicalPath}
  chunkDestinationFile = ${chunkDestinationFile.getCanonicalPath}
	  """)
      val file = request.body.file("file").get
      file.ref.moveTo(chunkDestinationFile, replace = true)

      // Check if we reached the end of the upload
      // Note: With flow.js prioritizeFirstAndLastChunk set to true the last chunk is received 
      // together with the first one thus before last is a good criteria to trigger upload end check
      if (flowTotalChunks == 1 || flowChunkNumber == (flowTotalChunks - 1)) {

        val retryNb = 3; val delayMillis = 3000L
        val uploadCompleted = FunctionsUtil.retry(retryNb, delayMillis) {
          Logger.debug(s"createElfinAnnexFile() - retry checkUploadComplete till $retryNb times with $delayMillis delay.")
          FileUploadHelper.checkUploadComplete(chunksSourceDirectory = chunksDirectory, fileIdentifier = flowIdentifier, totalChunks = flowTotalChunks, totalSize = flowTotalSize)
        }

        Logger.debug(s"createElfinAnnexFile() - uploadCompleted = $uploadCompleted")

        if (uploadCompleted) {

          // Creates file to write to 
          val finalUploadedFile = AnnexesManager.createElfinAnnexFile(elfinID_G = elfinID_G, elfinId = elfinId, fileName = flowFilename)

          // Writes chunks to final file
          FileUploadHelper.putChunksTogether(
            chunksSourceDirectory = chunksDirectory,
            resultFile = finalUploadedFile,
            fileIdentifier = flowIdentifier,
            totalChunks = flowTotalChunks,
            chunkSize = flowChunkSize,
            totalSize = flowTotalSize)

          // Perform cleanup task
          FileUploadHelper.deleteChunks(chunksSourceDirectory = chunksDirectory, fileIdentifier = flowIdentifier, totalChunks = flowTotalChunks)
        } else {
          // Try cleaning up failed upload
          FileUploadHelper.deleteChunks(chunksSourceDirectory = chunksDirectory, fileIdentifier = flowIdentifier, totalChunks = flowTotalChunks)
          throw FileUploadHelperUploadIncompleteException("Upload of file ${flowFilename} did not complete even after checking ${retryNb} times each delayed of ${delayMillis/1000} seconds.")
        }
      }
      Ok("""{ "message": "File uploaded"}""").as(JSON)
    } catch {
      case e: Exception => manageAnnexesManagerException(e)
    }
  }

  /**
    * Produce Docx document report from provided DOCX template and associated XQuery.
    */
  def getDocumentReport(filename:String): Action[AnyContent] = SecuredAction(ajaxCall = true).async { request =>
    val rawQueryString = if (request.rawQueryString.nonEmpty) Option(request.rawQueryString) else None

    XQueryWSHelper.getFile(filename).map { response =>

      if (response.ahcResponse.getStatusCode == 200) {

        // Convert document from inputstream to XWPFDocument object =========================================
        val doc: XWPFDocument = WordDocumentBuilder.getDocument(response.ahcResponse.getResponseBodyAsStream)

        // Get the result of the query as an HTML table =================================================
        val xqueryFileName = WordDocumentBuilder.getXQueryFileName(doc)
        // 10 minutes timeout is not expected to be reached but
        // is set to this very long value to avoid failing with
        // possibly very heavy xqueries.
        //
        // Non blocking solution seem not possible given
        // reportDynamicContentFuture depends on xqueryFileName
        import scala.concurrent.duration._
        val reportDynamicContent = Await.result(XQueryWSHelper.runXQueryFile(xqueryFileName, rawQueryString).map { response =>
          response.body.mkString
        }, 10 minutes)
        //Logger.debug("reportDynamicContent: " + reportDynamicContent)

        // Merge HTML table query result with workbook datasheet =========================================
        WordDocumentBuilder.mergeData(doc, reportDynamicContent)


        // Write document object back to an outputstream =================================================
        val out: ByteArrayOutputStream = new ByteArrayOutputStream()
        doc.write(out)
        out.close()
        val modifiedStream = new ByteArrayInputStream(out.toByteArray)

        // Sent the response stream ======================================================================
        Ok.chunked(Enumerator.fromStream(modifiedStream)).as(response.ahcResponse.getContentType)
      } else {
        val endUserMsg = s"Status: ${response.ahcResponse.getStatusCode} - ${response.ahcResponse.getStatusText}. Possible misconfiguration, please contact your system administrator."
        Logger.error(s"$endUserMsg Failing URI: ${response.ahcResponse.getUri}")
        ExceptionsManager.manageException(exception = None, errorMsg = Option(s"Failed to obtain file: $filename: $endUserMsg"))
      }
    }.recover {
      case e: Throwable =>
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain file: $filename: $e"))
    }

  }


  /**
   * Produce XLS spreadsheet report from provided XLS template and associated XQuery.
   */
  def getSpreadsheetReport(fileName: String): Action[AnyContent] = SecuredAction(ajaxCall = true).async { request =>

    val rawQueryString = if (request.rawQueryString != null && request.rawQueryString.nonEmpty) Option(request.rawQueryString) else None

    XQueryWSHelper.getFile(fileName).map { response =>

      if (response.ahcResponse.getStatusCode == 200) {

        // Convert workbook from inputstream to Workbook object =========================================
        val wb: Workbook = SpreadSheetBuilder.getWorkbook(response.ahcResponse.getResponseBodyAsStream)

        // Fill workbook parameter sheet with parameters values associated to xquery if any =============
        val queryStringMap = request.queryString

        SpreadSheetBuilder.updateParameterWorkBook(wb, queryStringMap)

        // Get the result of the query as an HTML table =================================================
        val xqueryFileName = SpreadSheetBuilder.getXQueryFileName(wb)
        val reportDynamicContentFuture = XQueryWSHelper.runXQueryFile(xqueryFileName, rawQueryString).map { response =>
          response.body.mkString
        }
        // 10 minutes timeout is not expected to be reached but 
        // is set to this very long value to avoid failing with 
        // possibly very heavy xqueries.
        // 
        // Non blocking solution seem not possible given 
        // reportDynamicContentFuture depends on xqueryFileName
        import scala.concurrent.duration._
        val reportDynamicContent = Await.result(reportDynamicContentFuture, 10 minutes)
        //Logger.debug("reportDynamicContent: " + reportDynamicContent)

        // Merge HTML table query result with workbook datasheet =========================================
        SpreadSheetBuilder.mergeHtmlTable(wb, reportDynamicContent)

        // SpreadSheetBuilder.insertWorkBookUserDetails(wb, request.user)
        // SpreadSheetBuilder.insertWorkBookPageNumbers(wb)
        SpreadSheetBuilder.evaluateAllFormulaCells(wb)

        // Write workbook object back to an outputstream =================================================
        val out: ByteArrayOutputStream = new ByteArrayOutputStream()
        wb.write(out)
        out.close()
        val modifiedStream = new ByteArrayInputStream(out.toByteArray)

        // Sent the response stream ======================================================================
        Ok.chunked(Enumerator.fromStream(modifiedStream)).as(response.ahcResponse.getContentType)
      } else {
        val endUserMsg = s"Status: ${response.ahcResponse.getStatusCode} - ${response.ahcResponse.getStatusText}. Possible misconfiguration, please contact your system administrator."
        Logger.error(s"$endUserMsg Failing URI: ${response.ahcResponse.getUri}")
        ExceptionsManager.manageException(exception = None, errorMsg = Option(s"Failed to obtain file: $fileName: $endUserMsg"))
      }
    }.recover {
      case e: Throwable =>
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain file: $fileName: $e"))
    }
  }



  /**
   * Returns the list of elfins contained in the specified collection matching the xpath filter expression with defined format
   */
  def getFilteredCollection(collectionId: String, xpath: String): Action[AnyContent] = SecuredAction(ajaxCall = true).async {
    XQueryWSHelper.query(WSQueries.filteredCollectionQuery(collectionId, xpath))
  }

  /**
   * Gets new ELFIN instance from catalog for provided CLASSE. This instance does not exist in database yet.
   */
  def getNewElfin(classeName: String): Action[AnyContent] = SecuredAction(ajaxCall = true).async { _ =>

    val futureElfinWithId: Future[ELFIN] = ElfinDAO.getNewFromCatalogue(classeName)

    // Send cloned catalog elfin in JSON format 
    futureElfinWithId.map { elfin =>
      // Intended no access right checks for reading entities from catalog 
      // These entities are read not only for creating new entities but also to 
      // obtain sensible defaults       
      val elfinJson = ElfinFormat.toJson(elfin)
      Ok(elfinJson).as(JSON)
    }.recover {
      case e: WithClasseEditRightException =>
        val errorMsg = s"Failed to obtain Elfin with CLASSE: $classeName from catalog: $e"
        manageWithClasseEditRightException(exception = e, errorMsg = Option(errorMsg))

      case resNotFound: ResultNotFoundException =>
        manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"Failed to obtain new ELFIN from catalog for classeName: $classeName: $resNotFound"))
      case connectException: ConnectException =>
        manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
      case e: Throwable =>
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain new ELFIN from catalog for classeName: $classeName: $e"))
    }
  }

  /**
   * Gets new ELFIN instance from catalog for provided CLASSE. This instance does not exist in database yet.
   */
  def getNewOrderNumber: Action[AnyContent] = SecuredAction(ajaxCall = true).async { _ =>

    val futureOrderNumber = ch.bsisa.hyperbird.orders.OrderNumberGenerator.getNewOrderNumber

    // Send cloned catalog elfin in JSON format 
    futureOrderNumber.map {
      case Some(orderNumber) => Ok(s"$orderNumber").as(JSON)
      case None => Ok("hello").as(JSON)
    }.recover {
      case connectException: ConnectException =>
        manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
      case e: Throwable =>
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain new order number from order ids service: $e"))
    }
  }

  /**
   * Updates an ELFIN.CARACTERISTIQUE.FRACTION.L block expecting a data structure designed for order figures.
   *
   * See COMMANDE catalog description for format details.
   */
  def computeOrderFigures(): Action[JsValue] = SecuredAction(ajaxCall = true).async(parse.json) { request =>

    Logger.debug(s"computeOrderFigures called by user: ${request.user}")

    try {
      // Convert elfin JsValue to ELFIN.CARACTERISTIQUE.FRACTION 
      val caracteristique = ElfinFormat.caracteristiqueFromJson(request.body)

      val updatedCaracteristique = ch.bsisa.hyperbird.orders.OrderUtil.computeOrderFigures(carP = caracteristique)
      val updatedCaracteristiqueJson = ElfinFormat.caracteristiqueToJson(updatedCaracteristique)
      scala.concurrent.Future(Ok(updatedCaracteristiqueJson).as(JSON))

    } catch {
      case e: Throwable =>
        val errorMsg = s"Failed to compute order figures: $e"
        ExceptionsManager.manageFutureException(exception = Option(e), errorMsg = Option(errorMsg))
    }

  }

  /**
   * Triggers asynchronous all database POINTs swiss federal coordinates (LV03) to GPS computation.
   */
  def convertAllPointsToGps(): Action[AnyContent] = SecuredAction(ajaxCall = true).async { _ =>
    import ch.bsisa.hyperbird.dao.GeoXmlDAO
    GeoXmlDAO.convertAllPointsToGps()
    scala.concurrent.Future(Ok("""{message:"All database POINTs swiss federal coordinates (LV03) to GPS computation triggered."}""").as(JSON))
  }

  /**
   * Finds 0 or 1 ELFIN by `collectionId`, `elfinId` and returns it within a SimpleResult
   *
   * Supported `format` parameter value are `{xml,json-pretty}`. Any other value will lead to `json` format.
   *
   */
  private def getElfinSimpleResult(collectionId: String, elfinId: String, format: String = JSON_FORMAT) = {

    format match {
      case XML_FORMAT =>
        val futureXmlElfin = XQueryWSHelper.findXml(WSQueries.elfinQuery(collectionId, elfinId))
        futureXmlElfin.map { elfin =>
          //val elfinJson = ElfinFormat.toJson(elfin)
          Ok(elfin).as(XML)
        }.recover {

          case resNotFound: ResultNotFoundException =>
            manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"No elfin found for ID_G: $collectionId, Id: $elfinId"))
          case connectException: ConnectException =>
            manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
          case e: Throwable =>
            ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Api.getElfinSimpleResult() - Failed to perform find operation for Elfin with ID_G: $collectionId, Id: $elfinId: $e"))
        }
      case JSON_PRETTY_FORMAT =>
        val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(collectionId, elfinId))
        futureElfin.map { elfin =>
          val elfinJson = ElfinFormat.toJson(elfin)
          Ok(Json.prettyPrint(elfinJson)).as(JSON)
        }.recover {
          case resNotFound: ResultNotFoundException =>
            manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"No elfin found for ID_G: $collectionId, Id: $elfinId"))
          case connectException: ConnectException =>
            manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
          case e: Throwable =>
            ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Api.getElfinSimpleResult() - Failed to perform find operation for Elfin with ID_G: $collectionId, Id: $elfinId: $e"))
        }
      case _ =>
        val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(collectionId, elfinId))
        futureElfin.map { elfin =>
          val elfinJson = ElfinFormat.toJson(elfin)
          Ok(elfinJson).as(JSON)
        }.recover {
          case resNotFound: ResultNotFoundException =>
            manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"No elfin found for ID_G: $collectionId, Id: $elfinId"))
          case connectException: ConnectException =>
            manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
          case e: Throwable =>
            ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Api.getElfinSimpleResult() - Failed to perform find operation for Elfin with ID_G: $collectionId, Id: $elfinId: $e"))
        }
    }

  }

  /**
   * Finds 0 or 1 ELFIN by `elfinId` and returns it within a SimpleResult
   */
  private def getElfinSimpleResult(elfinId: String) = {

    val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(elfinId))

    futureElfin.map { elfin =>
      val elfinJson = ElfinFormat.toJson(elfin)
      Ok(elfinJson).as(JSON)
    }.recover {
      case resNotFound: ResultNotFoundException =>
        manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"""No elfin found for Id: $elfinId"""))
      case connectException: ConnectException =>
        manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
      case e: Throwable =>
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Api.getElfinSimpleResult() - Failed to perform find operation for Elfin with Id: $elfinId: $e"))
    }
  }

  /**
   * Gets ELFIN corresponding to this `collectionId` and `elfinId`
   */
  def getElfin(collectionId: String, elfinId: String, format: String = "json"): Action[AnyContent] = SecuredAction(ajaxCall = true).async { implicit request =>
    Logger.debug(s"getElfin(collectionId=$collectionId, elfinId=$elfinId, format=$format) called by user: ${request.user}")
    getElfinSimpleResult(collectionId, elfinId, format)
  }

  /**
   * Gets ELFIN corresponding to this `elfinId`
   */
  def getElfinById(elfinId: String): Action[AnyContent] = SecuredAction(ajaxCall = true).async { implicit request =>
    Logger.debug(s"getElfin(elfinId=$elfinId) called by user: ${request.user}")
    getElfinSimpleResult(elfinId)
  }

  /**
   * Creates an ELFIN within the specified collectionId of CLASS className.
   */
  def createElfin(collectionId: String, elfinId: String): Action[JsValue] = SecuredAction(ajaxCall = true).async(parse.json) { request =>

    Logger.debug(s"createElfin(collectionId=$collectionId, elfinId=$elfinId) called by user: ${request.user}")

    try {
      // Match our custom User type Identity implementation  
      val user = request.user match { case user: User => user }

      // Convert elfin JsValue to ELFIN object and replace its ID_G with collectionId
      val elfin = ElfinUtil.replaceElfinID_G(elfin = ElfinFormat.fromJson(request.body), newElfinID_G = collectionId)

      // Application data based access right
      WithClasseEditRight.isAuthorized(user = user, elfinClasse = elfin.CLASSE)
      //elfin.IDENTIFIANT.get.GER
      WithManagerEditRight.isAuthorizedJs(elfin, request)

      // Test identifiers consistency between URL and JSON body
      if (elfin.Id.equals(elfinId)) {

        // Manage MUTATIONS.MUTATION head update
        val elfinWithUpdatedMutation = ElfinUtil.replaceElfinMutationsHead(elfin, ElfinUtil.createMutationForUserId(user.identityId.userId))

        // Update database with new elfinWithUpdatedMutation
        ElfinDAO.create(elfinWithUpdatedMutation)

        // Invalidate all cache entries related to this collectionId
        CacheHelper.removeEntriesContaining(collectionId)

        // Re-query the new ELFIN from the database as the only way we currently 
        // have to detect creation failure due to failing access rights or other issues.

        // Do not use query but find instead (encapsulated within private getElfinSimpleResult): 
        // 1) can return 0 - n instead of 0 -1 ELFIN.
        // 2) make easier to cache all query calls and no find calls.
        getElfinSimpleResult(collectionId, elfinId)
      } else {
        val errorMsg = s"PUT URL ELFIN ID_G/Id: $collectionId/$elfinId unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Creation cancelled."
        ExceptionsManager.manageFutureException(errorMsg = Option(errorMsg))
      }
    } catch {
      case e: WithManagerEditRightException =>
        val errorMsg = s"Failed to create Elfin with ID_G: $collectionId, Id: $elfinId: $e"
        manageFutureWithManagerEditRightException(exception = e, errorMsg = Option(errorMsg))
      case e: WithClasseEditRightException =>
        val errorMsg = s"Failed to create Elfin with ID_G: $collectionId, Id: $elfinId: $e"
        manageFutureWithClasseEditRightException(exception = e, errorMsg = Option(errorMsg))
      case e: Throwable =>
        val errorMsg = s"Failed to create Elfin with ID_G: $collectionId, Id: $elfinId: $e"
        ExceptionsManager.manageFutureException(exception = Option(e), errorMsg = Option(errorMsg))
    }

  }

  /**
   * Updates ELFIN within the specified collectionId with Id elfinId.
   * The data used to update this ELFIN will only be accepted if provided in JSON format.
   */
  // Kept as reminder but won't be used as for update, delete, create operations.
  //def updateElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true, authorize = WithRole("admin"))(parse.json) { request =>
  def updateElfin(collectionId: String, elfinId: String): Action[JsValue] = SecuredAction(ajaxCall = true)(parse.json) { request =>

    Logger.debug(s"updateElfin(collectionId=$collectionId, elfinId=$elfinId) called by user: ${request.user}")

    try {
      // Match our custom User type Identity implementation  
      val user = request.user match { case user: User => user }

      // Convert elfin JsValue to ELFIN object
      val elfin = ElfinFormat.fromJson(request.body)

      // Application data based access right
      WithClasseEditRight.isAuthorized(user = user, elfinClasse = elfin.CLASSE)
      WithManagerEditRight.isAuthorizedJs(elfin, request)

      // Test identifiers consistency between URL and JSON body
      if (elfin.ID_G.equals(collectionId) && elfin.Id.equals(elfinId)) {

        // Manage MUTATIONS.MUTATION head update
        val elfinWithUpdatedMutation = ElfinUtil.replaceElfinMutationsHead(elfin, ElfinUtil.createMutationForUserId(user.identityId.userId))

        // Update database with new elfin
        ElfinDAO.update(elfinWithUpdatedMutation)

        // Invalidate all cache entries related to this collectionId
        CacheHelper.removeEntriesContaining(collectionId)

        // Sent success response with updated elfin
        Ok(ElfinFormat.toJson(elfin)).as(JSON)
      } else {
        val errorMsg = s"PUT URL ELFIN ID_G/Id: $collectionId/$elfinId unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Update cancelled."
        ExceptionsManager.manageException(errorMsg = Option(errorMsg))
      }
    } catch {
      case e: WithManagerEditRightException =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: $collectionId, Id: $elfinId: $e"
        manageWithManagerEditRightException(exception = e, errorMsg = Option(errorMsg))
      case e: WithClasseEditRightException =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: $collectionId, Id: $elfinId: $e"
        manageWithClasseEditRightException(exception = e, errorMsg = Option(errorMsg))
      case e: Throwable =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: $collectionId, Id: $elfinId: $e"
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(errorMsg))
    }
  }

  /**
   * Delete the list of elfins contained in the specified collection matching the xpath filter expression with defined format
   */
  def deleteFilteredCollection(collectionId: String, xpath: String): Action[AnyContent] = SecuredAction(ajaxCall = true).async { request =>

    try {
      // Match our custom User type Identity implementation  
      val user = request.user match { case user: User => user }

      val futureElfins = XQueryWSHelper.queryElfins(WSQueries.filteredCollectionQuery(collectionId, xpath))
      futureElfins.map { elfins =>

        val simpleResults = elfins.map { elfin =>

          try {
            // Application data based access right
            WithClasseEditRight.isAuthorized(user = user, elfinClasse = elfin.CLASSE)
            WithManagerEditRight.isAuthorizedAny(elfin, request)

            // Delete elfin from database
            ElfinDAO.delete(elfin)

            val okMsg = Json.obj("DESCRIPTION" -> s"elfin.Id = ${elfin.Id}")
            Ok(okMsg)

          } catch {
            case e: WithManagerEditRightException =>
              val errorMsg = s"Failed to delete Elfins for collectionId: $collectionId, xpath: $xpath: $e"
              manageWithManagerEditRightException(exception = e, errorMsg = Option(errorMsg))
            case e: WithClasseEditRightException =>
              val errorMsg = s"Failed to delete Elfins for collectionId: $collectionId, xpath: $xpath: $e"
              manageWithClasseEditRightException(exception = e, errorMsg = Option(errorMsg))
            case e: Throwable =>
              val errorMsg = s"Api.deleteFilteredCollection() - Failed for collectionId: $collectionId, xpath: $xpath: $e"
              ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(errorMsg))
          }

        }
        val deletedElfinsStatus = simpleResults.find { s => s.header.status != Ok.header.status }
        deletedElfinsStatus match {
          case Some(simpleResultWithError) => simpleResultWithError
          case None =>
            val deletedElfinsMsg = s"""{ "message" :  "Api.deleteFilteredCollection() - success for collectionId: $collectionId, xpath: $xpath"  }"""
            Ok(deletedElfinsMsg).as(JSON)
        }

      }

    } catch {
      case e: Throwable =>
        val errorMsg = s"Api.deleteFilteredCollection() - Failed for collectionId: $collectionId, xpath: $xpath: $e"
        ExceptionsManager.manageFutureException(exception = Option(e), errorMsg = Option(errorMsg))
    }

  }

  /**
   * Deletes an ELFIN within the specified collectionId with Id elfinId.
   * RFC are not crystal clear regarding HTTP DELETE body usage or not but REST
   * principles states that URL information should uniquely identify a resource
   * to GET or DELETE.
   *
   * For that reason we do not process the request body for DELETE operation.
   * In addition trying to process it would fail with REST client such as
   * Restangular which does not send any body for DELETE operations.
   */
  def deleteElfin(collectionId: String, elfinId: String): Action[AnyContent] = SecuredAction(ajaxCall = true).async { request =>
    try {
      // Match our custom User type Identity implementation  
      val user = request.user match { case user: User => user }

      // Info level is fine for DELETE operation. They should not be frequent and should be easily traceable.
      Logger.info(s"deleteElfin(collectionId=$collectionId, elfinId=$elfinId) called by user: ${user.fullName} - ${user.identityId.userId}")

      // Make sure the resource we want to delete still exists.
      val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(collectionId, elfinId))
      futureElfin.map(elfin =>
        try {
          // Application data based access right
          WithClasseEditRight.isAuthorized(user = user, elfinClasse = elfin.CLASSE)
          WithManagerEditRight.isAuthorizedAny(elfin, request)

          // Delete elfin from database
          ElfinDAO.delete(elfin)

          // Send deleted elfin back to give a chance for cancellation (re-creation) 
          // provided the REST client does something with it unlike restangular
          Ok(ElfinFormat.toJson(elfin)).as(JSON)
        } catch {
          case e: WithManagerEditRightException =>
            val errorMsg = s"Failed to delete Elfin with ID_G: $collectionId, Id: $elfinId: $e"
            manageWithManagerEditRightException(exception = e, errorMsg = Option(errorMsg))
          case e: WithClasseEditRightException =>
            val errorMsg = s"Failed to delete Elfin with ID_G: $collectionId, Id: $elfinId: $e"
            manageWithClasseEditRightException(exception = e, errorMsg = Option(errorMsg))
          case e: Throwable =>
            ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Api.deleteElfin() - Failed to perform find operation for Elfin with ID_G: $collectionId, Id: $elfinId: $e"))
        })
    } catch {
      case e: Throwable =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: $collectionId, Id: $elfinId: $e"
        ExceptionsManager.manageFutureException(exception = Option(e), errorMsg = Option(errorMsg))
    }
  }

  // 566 - Custom code for connect exception
  def manageConnectException(exception: ConnectException, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> "db.connection.failure",
      "DESCRIPTION" -> errorMsg.getOrElse("").toString)
    Status(566)(jsonExceptionMsg)
  }

  /**
   * Encapsulate `manageConnectException` in an asynchronous call for use in Action.async context.
   * @see manageException
   */
  def manageFutureConnectException(exception: ConnectException, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageConnectException(exception, errorMsg) }

  // 567 - Custom code for ELFIN For exception ElfinFormatException
  def manageElfinFormatException(exception: ElfinFormatException, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> "elfin.format.failure",
      "DESCRIPTION" -> errorMsg.getOrElse("").toString,
      "ELFIN_Id" -> exception.elfinId,
      "ELFIN_ID_G" -> exception.elfinID_G)
    Status(567)(jsonExceptionMsg)
  }

  /**
   * Encapsulate `manageElfinFormatException` in an asynchronous call for use in Action.async context.
   * @see manageException
   */
  def manageFutureElfinFormatException(exception: ElfinFormatException, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageElfinFormatException(exception, errorMsg) }

  // 568 - Custom code for PasswordHashException
  def managePasswordHashException(exception: PasswordHashException, errorMsg: Option[String] = None): SimpleResult = {
    Logger.error("Api security exception: " + exception.toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> "security.passwordHash.failure",
      "DESCRIPTION" -> errorMsg.getOrElse("").toString)
    Status(568)(jsonExceptionMsg)
  }

  def manageAnnexesManagerException(exception: Exception, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.toString + " - " + errorMsg.getOrElse(""))
    exception match {
      case _: AnnexesManagerFileNotFoundException =>
        val jsonExceptionMsg = Json.obj(
          "ERROR" -> "annex.file.not.found",
          "DESCRIPTION" -> errorMsg.getOrElse("").toString)
        NotFound(jsonExceptionMsg)
      case _: AnnexesManagerCannotReadFileException =>
        val jsonExceptionMsg = Json.obj(
          "ERROR" -> "annex.file.cannot.read",
          "DESCRIPTION" -> errorMsg.getOrElse("").toString)
        Status(403)(jsonExceptionMsg) // 403 - Forbidden
      case _: Exception =>
        val jsonExceptionMsg = Json.obj(
          "ERROR" -> "annex.file.unexpected.exception",
          "DESCRIPTION" -> errorMsg.getOrElse("").toString)
        NotFound(jsonExceptionMsg)
    }

  }

  def manageResutlNotFoundException(exception: ResultNotFoundException, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> "no.result.found",
      "DESCRIPTION" -> errorMsg.getOrElse("").toString)
    NotFound(jsonExceptionMsg)
  }

  def manageWithClasseEditRightException(exception: WithClasseEditRightException, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> "security.exception.classe.edit.right",
      "DESCRIPTION" -> errorMsg.getOrElse("").toString)
    Status(403)(jsonExceptionMsg) // 403 - Forbidden
  }

  def manageWithManagerEditRightException(exception: WithManagerEditRightException, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> "security.exception.manager.edit.right",
      "DESCRIPTION" -> errorMsg.getOrElse("").toString)
    Status(403)(jsonExceptionMsg) // 403 - Forbidden
  }

  /**
   * Encapsulate `manageWithClasseEditRightException` in a asynchronous call for use in Action.async context.
   * @see manageWithClasseEditRightException
   */
  def manageFutureWithClasseEditRightException(exception: WithClasseEditRightException, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageWithClasseEditRightException(exception, errorMsg) }

  /**
   * Encapsulate `manageWithManagerEditRightException` in a asynchronous call for use in Action.async context.
   * @see manageWithManagerEditRightException
   */
  def manageFutureWithManagerEditRightException(exception: WithManagerEditRightException, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageWithManagerEditRightException(exception, errorMsg) }

  /**
   * Encapsulate `manageException` in a asynchronous call for use in Action.async context.
   * @see manageException
   */
  def manageFutureResutlNotFoundException(exception: ResultNotFoundException, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageResutlNotFoundException(exception, errorMsg) }
  def manageFutureException(exception: Option[Throwable] = None, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { ExceptionsManager.manageException(exception, errorMsg) }

  /**
   * Gets ELFIN corresponding to this collectionId and elfinId
   */
  def getReport(reportCollectionId: String, reportElfinId: String): Action[AnyContent] = SecuredAction(ajaxCall = true).async { request =>

    // Flatten Map[String, Seq[String]] to a Map[String, String]
    val queryStringMap: Map[String, String] = request.queryString.map { case (key, value) => key -> value.mkString }
    val queryString: Option[String] = if (request.rawQueryString != null && request.rawQueryString.nonEmpty) Option(request.rawQueryString) else None
    Logger.debug(s"getReport(reportCollectionId=$reportCollectionId, reportElfinId=$reportElfinId) called, queryString = ${queryString.getOrElse("")}")

    XQueryWSHelper.find(WSQueries.elfinQuery(reportCollectionId, reportElfinId)).flatMap { reportElfin =>
      ReportBuilder.writeReport(reportElfin, queryString, Some(queryStringMap))
    }.map { tempFile =>
      Ok.sendFile(tempFile.file, inline = true)
    }.recover {
      case resNotFound: ResultNotFoundException =>
        manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"No elfin found for ID_G: $reportCollectionId, Id: $reportElfinId"))
      case connectException: ConnectException =>
        manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
      case e: Throwable =>
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Api.getReport() - Failed to perform find operation for Elfin with ID_G: $reportCollectionId, Id: $reportElfinId: $e"))
    }
  }

  /**
    * Gets ELFIN corresponding to this collectionId and elfinId
    */
  def getAnnexesOnlyReport(annexType:String, reportCollectionId: String, reportElfinId: String): Action[AnyContent] = SecuredAction(ajaxCall = true).async { request =>

    // Flatten Map[String, Seq[String]] to a Map[String, String]
    val queryStringMap: Map[String, String] = request.queryString.map { case (key, value) => key -> value.mkString }
    val queryString: Option[String] = if (request.rawQueryString != null && request.rawQueryString.nonEmpty) Option(request.rawQueryString) else None
    Logger.debug(s"getReport(annexType=$annexType, reportCollectionId=$reportCollectionId, reportElfinId=$reportElfinId) called, queryString = ${queryString.getOrElse("")}")

    XQueryWSHelper.find(WSQueries.elfinQuery(reportCollectionId, reportElfinId)).map { reportElfin =>

      val reportFile = ReportBuilder.writeAnnexeReport(reportElfin, queryString, Some(queryStringMap), annexType)
      if (reportFile == null) {
        val e= new IllegalStateException()
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Api.getReport() - Failed to perform find operation for Elfin with ID_G: $reportCollectionId, Id: $reportElfinId: $e"))
      } else {
        Ok.sendFile(reportFile.file, inline = true)
      }
    }
  }
}

