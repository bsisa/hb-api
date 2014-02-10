package ch.bsisa.hyperbird.controllers

import play.api._
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.mvc._
import scala.concurrent.Future
import scala.xml.XML
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.dao.xqs.XQSQueriesProcessor
import ch.bsisa.hyperbird.dao.xqs.XQSQueries
import ch.bsisa.hyperbird.dao.UserDAO
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.util.ElfinIdGenerator
import ch.bsisa.hyperbird.model.ELFIN
import play.api.libs.json.Json
import ch.bsisa.hyperbird.util.ElfinUtil

/**
 * REST API controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 * @author Guy de Pourtales
 */
object Api extends Controller {

  /**
   * TODO: review specifications. Listing collections.
   * <ul>
   * <li>Do we really have any use case for this ?</li>
   * <li>What type of data to return in geoXml.xsd world ?</li>
   * </ul>
   */
  def collections = Action.async {
    XQueryWSHelper.query(WSQueries.allHbCollectionsQuery)
  }

  /**
   * Returns the list of elfins contained in the specified collection matching the xpath filter expression with defined format
   *
   * TODO: make use of format parameter value, currently returns JSON format only. (format=(json|xml|pdf|xls|...)
   */
  def filteredCollection(collectionId: String, xpath: String, format: String) = Action.async {
    Logger.warn(s"TODO: make use of format parameter value ${format}")
    XQueryWSHelper.query(WSQueries.filteredCollectionQuery(collectionId, xpath))
  }

  /**
   * Gets new ELFIN instance from catalogue for provided CLASSE. This instance do not exist in database yet.
   */
  def getNewElfin(classeName: String) = Action.async {
    // TODO: make this configurable. (In hb_init ? Itself found in catalogueCollectionId at the moment!!!)
    val catalogueCollectionId = "G20140101000012345"

    // Use generic find query with catalogue collection id and ELFIN@CLASSE parameter 
    val futureElfin = XQueryWSHelper.find(
      WSQueries.filteredCollectionQuery(catalogueCollectionId, s"//ELFIN[@CLASSE='${classeName}']"))

    // Clone futureElfin[ELFIN] and assign a new generated ELFIN.Id to it
    val futureElfinWithId: Future[ELFIN] = futureElfin.map(elfin => ElfinUtil.assignElfinId(elfin))

    futureElfinWithId.map(elfin =>
      try {
        val elfinJson = ElfinFormat.toJson(elfin)
        Ok(elfinJson).as(JSON)
      } catch {
        case e: Throwable =>
          manageException(exception = Option(e))
      })
  }

  /**
   * Gets ELFIN corresponding to this collectionId and elfinId
   */
  def getElfin(collectionId: String, elfinId: String) = Action.async {

    val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(collectionId, elfinId))

    futureElfin.map(elfin =>
      try {
        val elfinJson = ElfinFormat.toJson(elfin)
        Ok(elfinJson).as(JSON)
      } catch {
        case e: Throwable =>
          manageException(exception = Option(e), errorMsg = Option(s"Failed to perform find operation for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"))
      })
  }

  /**
   * Creates an ELFIN within the specified collectionId of CLASS className.
   */
  def createElfin(collectionId: String, elfinId: String) = Action.async(parse.json) { request =>

    try {
      // Convert elfin JsValue to ELFIN object and replace its ID_G with collectionId
      val elfin = ElfinUtil.replaceElfinID_G(elfin = ElfinFormat.fromJson(request.body), newElfinID_G = collectionId)

      // Test identifiers consistency between URL and JSON body
      if (elfin.Id.equals(elfinId)) {
        // Update database with new elfin
        ElfinDAO.create(elfin)
        // Re-query the new ELFIN from the database as the only way we currently 
        // have to detect creation failure due to failing access rights or other issues.
        XQueryWSHelper.query(WSQueries.elfinQuery(collectionId, elfinId))
      } else {
        val errorMsg = s"PUT URL ELFIN ID_G/Id: ${collectionId}/${elfinId} unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Creation cancelled."
        manageFutureException(errorMsg = Option(errorMsg))
      }
    } catch {
      case e: Throwable =>
        manageFutureException(exception = Option(e), errorMsg = Option(s"Failed to perform creation for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"))
    }

  }

  /**
   * Updates ELFIN within the specified collectionId with Id elfinId.
   * The data used to update this ELFIN will only be accepted if provided in JSON format.
   */
  def updateElfin(collectionId: String, elfinId: String) = Action(parse.json) { request =>
    try {
      // Convert elfin JsValue to ELFIN object
      val elfin = ElfinFormat.fromJson(request.body)

      // Test identifiers consistency between URL and JSON body
      if (elfin.ID_G.equals(collectionId) && elfin.Id.equals(elfinId)) {
        // Update database with new elfin
        ElfinDAO.update(elfin)
        // Sent success response
        //Ok(s"""{"message": "elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id} update successful"}""").as(JSON)
        Ok(ElfinFormat.toJson(elfin)).as(JSON)
      } else {
        val errorMsg = s"PUT URL ELFIN ID_G/Id: ${collectionId}/${elfinId} unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Update cancelled."
        manageException(errorMsg = Option(errorMsg))
      }
    } catch {
      case e: Throwable =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        manageException(exception = Option(e), errorMsg = Option(errorMsg))
    }
  }

  /**
   * Updates ELFIN within the specified collectionId with Id elfinId
   */
  def deleteElfin(collectionId: String, elfinId: String) = Action(parse.json) { request =>
    try {
      // Convert elfin JsValue to ELFIN object
      val elfin = ElfinFormat.fromJson(request.body)

      // Test identifiers consistency between URL and JSON body
      if (elfin.ID_G.equals(collectionId) && elfin.Id.equals(elfinId)) {
        // Delete elfin from database
        ElfinDAO.delete(elfin)
        // Send deleted elfin back to give a chance for cancellation (re-creation)
        Ok(ElfinFormat.toJson(elfin)).as(JSON)
      } else {
        val errorMsg = s"DELETE URL ELFIN ID_G/Id: ${collectionId}/${elfinId} unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Deletion cancelled."
        manageException(errorMsg = Option(errorMsg))
      }
    } catch {
      case e: Throwable =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        manageException(exception = Option(e), errorMsg = Option(errorMsg))
    }
  }


  /**
   * Utility method to return exception, error message in a generic JSON error message.
   */
  private def manageException(exception: Option[Throwable] = None, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.getOrElse("").toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> exception.getOrElse("application.validation.failure").toString,
      "DESCRIPTION" -> errorMsg.getOrElse(exception.getOrElse("None").toString).toString // TODO: review
      )
    InternalServerError(jsonExceptionMsg).as(JSON)
  }

  /**
   * Encapsulate `manageException` in a asynchronous call for use in Action.async context.
   * @see manageException
   */
  private def manageFutureException(exception: Option[Throwable] = None, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageException(exception, errorMsg) }

}
