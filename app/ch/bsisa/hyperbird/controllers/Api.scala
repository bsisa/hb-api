package ch.bsisa.hyperbird.controllers

import ch.bsisa.hyperbird.CollectionsConfig
import ch.bsisa.hyperbird.InitConfig
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.dao.ResultNotFoundException
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.model.format.ElfinFormat.ElfinFormatException
import ch.bsisa.hyperbird.security.HbSecureService
import ch.bsisa.hyperbird.security.HbSecureService.PasswordHashException
import ch.bsisa.hyperbird.spreadsheet.SpreadSheetBuilder
import ch.bsisa.hyperbird.util.ElfinUtil
import org.apache.poi.ss.usermodel._
import securesocial.core.java.SecureSocial.SecuredAction
import play.api._
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.mvc._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.xml.XML
import java.net.ConnectException
import java.io.InputStream
import java.io.OutputStream
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import ch.bsisa.hyperbird.security.WithRole
import ch.bsisa.hyperbird.security.WithClasseEditRight
import ch.bsisa.hyperbird.security.WithClasseEditRightException
import securesocial.core.SocialUser
import ch.bsisa.hyperbird.security.User



/**
 * REST API controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 * @author Guy de Pourtales
 */
object Api extends Controller with securesocial.core.SecureSocial {

  val JSON_FORMAT = "json"
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
   * Dynamically provides initialisation configuration information useful to hb-ui
   */
  def config() = SecuredAction(ajaxCall = true) {
    Ok(getConfigJson).as(JSON)
  }

  /**
   * TODO: review specifications. Listing collections.
   * <ul>
   * <li>Do we really have any use case for this ?</li>
   * <li>What type of data to return in geoXml.xsd world ?</li>
   * </ul>
   */
  def collections = SecuredAction(ajaxCall = true).async {
    XQueryWSHelper.query(WSQueries.allHbCollectionsQuery)
  }

  
  
  /**
   * Provides password hashing service.
   */
  def getPasswordHash(plainTextPassword : String) = SecuredAction(ajaxCall = true) { request => 
    //
    try {
	    val plainTextPasswordHashValue = HbSecureService.getPasswordHash(plainTextPassword) 
	    val jsonResponse = 
	      s"""{
	      "hash" : "${plainTextPasswordHashValue}"
}"""  
	    Ok(jsonResponse).as(JSON)
    } catch {
      case phe: PasswordHashException => {
        managePasswordHashException(exception = phe, errorMsg = Option(s"User: ${request.user} failed to obtain password hash."))
      }
      case e: Throwable => {
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"User: ${request.user} failed to obtain password hash."))
      }
    }

    
  }

  
  /**
   * Returns the result of executing the specified XQuery file by name.
   *
   * Supported `format` parameter value are `{original|json}`
   */
  def runXQueryFile(xqueryFileName: String, format: String) = SecuredAction(ajaxCall = true).async { request =>
    val queryString = if (request.rawQueryString != null && request.rawQueryString.nonEmpty) Option(request.rawQueryString) else None
    Logger.debug(s"Run XQuery ${xqueryFileName} with returned format = ${format} and rawQueryString: ${queryString}")

    XQueryWSHelper.runXQueryFile(xqueryFileName, queryString).map { response =>
      format match {
        case JSON_FORMAT =>
          Logger.debug(s">>>> runXQueryFile: Result of type ${response.ahcResponse.getContentType} received")
          val melfinWrappedBody = "<MELFIN>" + response.body.mkString + "</MELFIN>"
          Ok(ElfinFormat.elfinsToJsonArray(ElfinFormat.elfinsFromXml(scala.xml.XML.loadString(melfinWrappedBody))))
        case ORIGINAL_FORMAT => Ok(response.body).as(response.ahcResponse.getContentType())
      }
    }
  }

  /**
   * Produce XLS spreadsheet report from provided XLS template and associated XQuery.
   */
  def getSpreadsheetReport(fileName: String) = SecuredAction(ajaxCall = true).async { request =>

    val rawQueryString = if (request.rawQueryString != null && request.rawQueryString.nonEmpty) Option(request.rawQueryString) else None

    XQueryWSHelper.getFile(fileName).map { response =>
      
      if (response.ahcResponse.getStatusCode() == 200) {

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

      SpreadSheetBuilder.insertWorkBookUserDetails(wb, request.user)
      SpreadSheetBuilder.insertWorkBookPageNumbers(wb)
      SpreadSheetBuilder.evaluateAllFormulaCells(wb)
      
      // Write workbook object back to an outputstream =================================================
      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      wb.write(out)
      out.close()
      val modifiedStream = new ByteArrayInputStream(out.toByteArray)

      // Sent the response stream ======================================================================
      Ok.chunked(Enumerator.fromStream(modifiedStream)).as(response.ahcResponse.getContentType)
      } else {
        val endUserMsg = s"Status: ${response.ahcResponse.getStatusCode()} - ${response.ahcResponse.getStatusText()}. Possible misconfiguration, please contact your system administrator."
        Logger.error(s"${endUserMsg} Failing URI: ${response.ahcResponse.getUri()}")
        ExceptionsManager.manageException(exception = None, errorMsg = Option(s"Failed to obtain file: ${fileName}: ${endUserMsg}"))
      }
    }.recover {
      case e: Throwable => {
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain file: ${fileName}: ${e}"))
      }
    }
  }

  /**
   * Returns the list of elfins contained in the specified collection matching the xpath filter expression with defined format
   *
   * TODO: make use of format parameter value, currently returns JSON format only. (format=(json|xml|pdf|xls|...)
   */
  def filteredCollection(collectionId: String, xpath: String, format: String) = SecuredAction(ajaxCall = true).async {
    Logger.warn(s"TODO: make use of format parameter value ${format}")
    XQueryWSHelper.query(WSQueries.filteredCollectionQuery(collectionId, xpath))
  }

  /**
   * Gets new ELFIN instance from catalogue for provided CLASSE. This instance does not exist in database yet.
   */
  def getNewElfin(classeName: String) = SecuredAction(ajaxCall = true).async { request =>

    val futureElfinWithId: Future[ELFIN] = ElfinDAO.getNewFromCatalogue(classeName)

    // Send cloned catalogue elfin in JSON format 
    futureElfinWithId.map { elfin =>
      
      // Application data based access right
      // TODO: Review: Prevents to obtain default from catalogue entity even for read only mode
      //WithClasseEditRight.isAuthorized(user = request.user, elfinClasse = elfin.CLASSE)      
      
      val elfinJson = ElfinFormat.toJson(elfin)
      Ok(elfinJson).as(JSON)
    }.recover {
      case e: WithClasseEditRightException =>
        val errorMsg = s"Failed to obtain Elfin with CLASSE: ${classeName} from catalogue: ${e}"
        manageWithClasseEditRightException(exception = e, errorMsg = Option(errorMsg))      
      
      case resNotFound: ResultNotFoundException => {
        manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"Failed to obtain new ELFIN from catalogue for classeName: ${classeName}: ${resNotFound}"))
      }
      case connectException: ConnectException => {
        manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
      }
      case e: Throwable => {
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain new ELFIN from catalogue for classeName: ${classeName}: ${e}"))
      }
    }
  }

  /**
   * Gets ELFIN corresponding to this collectionId and elfinId
   */
  def getElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true).async { implicit request =>

    Logger.debug(s"getElfin(collectionId=${collectionId}, elfinId=${elfinId}) called by user: ${request.user}")

    val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(collectionId, elfinId))

    futureElfin.map { elfin =>
      val elfinJson = ElfinFormat.toJson(elfin)
      Ok(elfinJson).as(JSON)
    }.recover {
      case resNotFound: ResultNotFoundException => {
        manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"No elfin found for ID_G: ${collectionId}, Id: ${elfinId}"))
      }
      case connectException: ConnectException => {
        manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
      }
      case e: Throwable => {
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to perform find operation for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"))
      }
    }
  }

  /**
   * Creates an ELFIN within the specified collectionId of CLASS className.
   */
  def createElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true).async(parse.json) { request =>

    Logger.debug(s"createElfin(collectionId=${collectionId}, elfinId=${elfinId}) called by user: ${request.user}")
    
    try {
      // Match our custom User type Identity implementation  
      val user = request.user match { case user: User => user }
      
      // Convert elfin JsValue to ELFIN object and replace its ID_G with collectionId
      val elfin = ElfinUtil.replaceElfinID_G(elfin = ElfinFormat.fromJson(request.body), newElfinID_G = collectionId)

      // Application data based access right
      WithClasseEditRight.isAuthorized(user = user, elfinClasse = elfin.CLASSE)
      
      // Test identifiers consistency between URL and JSON body
      if (elfin.Id.equals(elfinId)) {
        // Update database with new elfin
        ElfinDAO.create(elfin)
        // Re-query the new ELFIN from the database as the only way we currently 
        // have to detect creation failure due to failing access rights or other issues.
        XQueryWSHelper.query(WSQueries.elfinQuery(collectionId, elfinId))
      } else {
        val errorMsg = s"PUT URL ELFIN ID_G/Id: ${collectionId}/${elfinId} unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Creation cancelled."
        ExceptionsManager.manageFutureException(errorMsg = Option(errorMsg))
      }
    } catch {
      case e: WithClasseEditRightException =>
        val errorMsg = s"Failed to create Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        manageFutureWithClasseEditRightException(exception = e, errorMsg = Option(errorMsg))
      case e: Throwable =>
        val errorMsg = s"Failed to create Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        ExceptionsManager.manageFutureException(exception = Option(e), errorMsg = Option(errorMsg))
    }

  }

  /**
   * Updates ELFIN within the specified collectionId with Id elfinId.
   * The data used to update this ELFIN will only be accepted if provided in JSON format.
   */
  // Kept as reminder but won't be used as for update, delete, create operations.
  //def updateElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true, authorize = WithRole("admin"))(parse.json) { request =>
  def updateElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true)(parse.json) { request =>    
    
    Logger.debug(s"updateElfin(collectionId=${collectionId}, elfinId=${elfinId}) called by user: ${request.user}")
    
    try {
      // Match our custom User type Identity implementation  
      val user = request.user match { case user: User => user }
      
      // Convert elfin JsValue to ELFIN object
      val elfin = ElfinFormat.fromJson(request.body)

      // Application data based access right
      WithClasseEditRight.isAuthorized(user = user, elfinClasse = elfin.CLASSE)      
      
      // Test identifiers consistency between URL and JSON body
      if (elfin.ID_G.equals(collectionId) && elfin.Id.equals(elfinId)) {
        // Update database with new elfin
        ElfinDAO.update(elfin)
        // Sent success response with updated elfin
        Ok(ElfinFormat.toJson(elfin)).as(JSON)
      } else {
        val errorMsg = s"PUT URL ELFIN ID_G/Id: ${collectionId}/${elfinId} unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Update cancelled."
        ExceptionsManager.manageException(errorMsg = Option(errorMsg))
      }
    } catch {
      case e: WithClasseEditRightException =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        manageWithClasseEditRightException(exception = e, errorMsg = Option(errorMsg))
      case e: Throwable =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(errorMsg))
    }
  }

  /**
   * Deletes an ELFIN within the specified collectionId with Id elfinId.
   * RFC are not cristal clear regarding HTTP DELETE body usage or not but REST
   * principles states that URL information should uniquely identify a resource
   * to GET or DELETE.
   *
   * For that reason we do not process the request body for DELETE operation.
   * In addition trying to process it would fail with REST client such as
   * Restangular which does not sent any body for DELETE operations.
   */
  def deleteElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true).async { request =>
    try {
      // Match our custom User type Identity implementation  
      val user = request.user match { case user: User => user }
      
      // Info level is fine for DELETE operation. They should not be frequent and should be easily traceable.
      Logger.info(s"deleteElfin(collectionId=${collectionId}, elfinId=${elfinId}) called by user: ${user.fullName} - ${user.identityId.userId}")
      
      // Make sure the resource we want to delete still exists.
      val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(collectionId, elfinId))
      futureElfin.map(elfin =>
        try {
	      // Application data based access right
	      WithClasseEditRight.isAuthorized(user = user, elfinClasse = elfin.CLASSE)
          // Delete elfin from database
          ElfinDAO.delete(elfin)
          // Send deleted elfin back to give a chance for cancellation (re-creation) 
          // provided the REST client does something with it unlike restangular
          Ok(ElfinFormat.toJson(elfin)).as(JSON)
        } catch {
          case e: WithClasseEditRightException =>
        	val errorMsg = s"Failed to delete Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        	manageWithClasseEditRightException(exception = e, errorMsg = Option(errorMsg))          
          case e: Throwable =>
            ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to perform find operation for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"))
        })
    } catch {
      case e: Throwable =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
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
      "ELFIN_Id" ->  exception.elfinId,
      "ELFIN_ID_G" ->  exception.elfinID_G)
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
  
  /**
   * Encapsulate `manageWithClasseEditRightException` in a asynchronous call for use in Action.async context.
   * @see manageWithClasseEditRightException
   */
  def manageFutureWithClasseEditRightException(exception: WithClasseEditRightException, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageWithClasseEditRightException(exception, errorMsg) }  
  
  /**
   * Encapsulate `manageException` in a asynchronous call for use in Action.async context.
   * @see manageException
   */
  def manageFutureResutlNotFoundException(exception: ResultNotFoundException, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageResutlNotFoundException(exception, errorMsg) }

}
