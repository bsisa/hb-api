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
import ch.bsisa.hyperbird.util.JsonXmlConverter
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.dao.xqs.XQSQueriesProcessor
import ch.bsisa.hyperbird.dao.xqs.XQSQueries
import ch.bsisa.hyperbird.dao.UserDAO
import ch.bsisa.hyperbird.dao.ElfinDAO

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
  def createElfin(collectionId: String, className: String) = Action {
    Ok("{message: not implemented}").as(JSON)
  }

  /**
   * Updates ELFIN within the specified collectionId with Id elfinId.
   * The data used to update this ELFIN will only be accepted if provided in JSON format.
   */
  def updateElfin(collectionId: String, elfinId: String) = Action(parse.json) { request =>
    Logger.debug(s"request.contentType: ${request.contentType}")
    Logger.debug(s"request.body: ${request.body}")
    ElfinDAO.update(collectionId, elfinId, JsonXmlConverter.jsonStringToXml(request.body.toString))
    
    Ok("{message: not implemented}").as(JSON)
  }

  /**
   * Updates ELFIN within the specified collectionId with Id elfinId
   */
  def deleteElfin(collectionId: String, elfinId: String) = Action {
    Ok("{message: not implemented}").as(JSON)
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
