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
   * TODO: review specifications. Listing collections
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
   * Gets ELFIN corresponding to this collectionId and elfinId
   */
  def getElfin(collectionId: String, elfinId: String) = Action.async {
    XQueryWSHelper.query(WSQueries.elfinQuery(collectionId, elfinId))
  }

  /**
   * Creates an ELFIN within the specified collectionId of CLASS className.
   */
  def createElfin(collectionId: String, className: String) = Action { request =>
    Logger.debug("CREATE ELFIN REQUEST: " + request.toString)
    ElfinDAO.create(collectionId,className)
    Ok("""{"message": "draft implementation returns nothing at the momemnt..."}""").as(JSON)
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
        Ok(s"""{"message": "elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id} update successful"}""").as(JSON)
      } else {
        val errorMsg = s"PUT URL ELFIN ID_G/Id: ${collectionId}/${elfinId} unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Update cancelled."
        Logger.warn(errorMsg)
        // Sent failure response following identifiers inconsistency detection 
        InternalServerError(errorMsg)
      }
    } catch {
      case e: Throwable =>
        val errorMsg = s"Failed to perform updateElfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        Logger.warn(errorMsg)
        // Sent failure response following json to object, database operation or any other exception. 
        InternalServerError(errorMsg)
    }

  }

  /**
   * Updates ELFIN within the specified collectionId with Id elfinId
   */
  def deleteElfin(collectionId: String, elfinId: String) = Action {
    Ok("""{"message": "not implemented"}""").as(JSON)
  }

  def createUser() = Action {
    UserDAO.create
    Ok("{message: 'createUser test completed'}").as(JSON)
  }

  def updateUser() = Action {
    UserDAO.update
    Ok("{message: 'updateUser test completed'}").as(JSON)
  }

  def deleteUser() = Action {
    UserDAO.delete
    Ok("{message: 'deleteUser test completed'}").as(JSON)
  }

  def findUser(userName: String) = Action {
    // Convert the XML result to JSON format
    val jsonSeqElem = JsonXmlConverter.xmlSeqToJson(UserDAO.find(userName))
    // Produce a SimpleResult
    Ok(jsonSeqElem).as(JSON)
  }

}
