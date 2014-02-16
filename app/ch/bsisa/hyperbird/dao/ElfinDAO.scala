package ch.bsisa.hyperbird.dao

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.xqs.XQConnectionHelper
import ch.bsisa.hyperbird.dao.xqs.XQueryHelper
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.util.ElfinIdGenerator
import scala.concurrent.Future
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.CollectionsConfig
import ch.bsisa.hyperbird.util.ElfinUtil

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._

/**
 * Provides CRUD operations for ELFIN
 *
 * @author Patrick Refondini
 */
object ElfinDAO {

  /**
   * Creates XML database ELFIN representation corresponding to the provided ELFIN object
   * into the database.
   * <i>The database API do not provide any feedback on that operation.</i>
   */
  def create(elfin: ELFIN)(implicit dbConfig: DbConfig) = {
    Logger.debug(s"ElfinDAO.create elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id}")
    XQueryWSHelper.create(elfin)
  }

  /**
   * Updates XML database ELFIN representation corresponding to the provided ELFIN object.
   * <i>The database API do not provide any feedback on that operation.</i>
   */
  def update(elfin: ELFIN)(implicit dbConfig: DbConfig) = {
    Logger.debug(s"ElfinDAO.update elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id}")
    val elfinXml = ElfinFormat.toXml(elfin)
    val updateStatetement =
      s"update replace collection('${dbConfig.databaseName}/${elfin.ID_G}')//ELFIN[@Id='${elfin.Id}'] with ${elfinXml.mkString}"
    executeStatement(updateStatetement)
  }

  /**
   * Deletes XML database ELFIN representation corresponding to the provided ELFIN object.
   * <i>The database API do not provide any feedback on that operation.</i>
   */
  def delete(elfin: ELFIN)(implicit dbConfig: DbConfig) = {
    Logger.debug(s"ElfinDAO.delete elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id}")
    XQueryWSHelper.delete(elfin)
  }

  /**
   * Gets new ELFIN instance from catalogue for provided CLASSE and assign a new ELFIN.Id to it.
   * Note that this instance does not exist in database. 
   * To persist a new ELFIN instance to database use `ElfinDAO.create`.
   */
  def getNewFromCatalogue(classeName: String)(implicit dbConfig: DbConfig, collectionsConfig: CollectionsConfig): Future[ELFIN] = {
    // Use generic find query with catalogue collection id and ELFIN@CLASSE parameter 
    val futureElfin = XQueryWSHelper.find(
      WSQueries.filteredCollectionQuery(collectionsConfig.catalogueCollectionId, s"//ELFIN[@CLASSE='${classeName}']"))

    // Clone futureElfin[ELFIN] and assign a new generated ELFIN.Id to it
    val futureElfinWithId: Future[ELFIN] = futureElfin.map(elfin => ElfinUtil.assignElfinId(elfin))
    futureElfinWithId
  }

  /**
   * Helper function designed to executes XUpdate statements
   *
   * Note: do not use prepareExpression(statement)
   * it will fail to validate XUpdate instructions.
   * Only valid XQuery syntax seems currently supported.
   */
  private def executeStatement(statement: String)(implicit conf: DbConfig) = {
    val conn = XQConnectionHelper.getConnection()(conf)
    try {
      Logger.debug("executeStatement(statement) : " + statement)
      var xqe = conn.createExpression()
      xqe.executeCommand(statement)
    } finally {
      conn.close()
    }
  }

}