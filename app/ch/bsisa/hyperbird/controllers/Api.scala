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
import ch.bsisa.hyperbird.dao.TestQueries
import ch.bsisa.hyperbird.dao.XQueryHelper
import ch.bsisa.hyperbird.util.JsonXmlConverter
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.dao.xqs.XQSQueriesProcessor
import ch.bsisa.hyperbird.dao.xqs.XQSQueries

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

  def collection(collectionId: String) = Action.async {
    XQueryWSHelper.query(WSQueries.fileteredCollectionQuery(collectionId))
  }

  def fileteredCollection(collectionId: String, xpath: String) = Action.async {
    XQueryWSHelper.query(WSQueries.fileteredCollectionQuery(collectionId, xpath))
  }

  def elfinWithinCollection(collectionId: String, elfinId: String) = Action.async {
    XQueryWSHelper.query(WSQueries.elfinQuery(collectionId, elfinId))
  }

  def elfin(elfinId: String) = Action.async {
    XQueryWSHelper.query(WSQueries.elfinQuery(elfinId))
  }

}