package ch.bsisa.hyperbird.dao

import play.api.Logger
import ch.bsisa.hyperbird.dao.xqs.XQConnectionHelper
import ch.bsisa.hyperbird.dao.xqs.XQueryHelper
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.util.ElfinIdGenerator

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
  def create(elfin: ELFIN)(implicit conf: DbConfig) = {
    XQueryWSHelper.create(elfin)
  }

  /**
   * Updates XML database ELFIN representation corresponding to the provided ELFIN object.
   * <i>The database API do not provide any feedback on that operation.</i>
   */
  def update(elfin: ELFIN)(implicit conf: DbConfig) = {
    Logger.debug(s"Updating elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id}")
    val elfinXml = ElfinFormat.toXml(elfin)
    val updateStatetement =
      s"update replace collection('${conf.databaseName}/${elfin.ID_G}')//ELFIN[@Id='${elfin.Id}'] with ${elfinXml.mkString}"
    executeStatement(updateStatetement)
  }

  /**
   * Deletes XML database ELFIN representation corresponding to the provided ELFIN object.
   * <i>The database API do not provide any feedback on that operation.</i>
   */  
  def delete(elfin: ELFIN)(implicit conf: DbConfig) = {
    Logger.debug(s"Updating elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id}")
    val elfinXml = ElfinFormat.toXml(elfin)
    val deleteStatetement =
      s"update delete collection('${conf.databaseName}/${elfin.ID_G}')//ELFIN[@Id='${elfin.Id}']"
    executeStatement(deleteStatetement)
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

  /////////////////////////////////////////////////////////////////////
  //////////////    hb-user tests TO BE DELETED   /////////////////////
  /////////////////////////////////////////////////////////////////////

  def find(userName: String)(implicit conf: DbConfig): Seq[scala.xml.Elem] = {
    val query = s"""collection('${conf.databaseName}/security')/hb-users/hb-user[@name='${userName}']"""
    // Perform call to eXist via XQS/XQJ
    XQueryHelper.seqOfElem(query)
  }

  //TODO: full implementation required
  def delete()(implicit conf: DbConfig) = {
    val insertStatetement = s"""for $$hbuser in collection('${conf.databaseName}/security')/hb-users/hb-user[@name='test1']
return
    update delete $$hbuser"""
    executeStatement(insertStatetement)
  }

}