package ch.bsisa.hyperbird.dao

import play.api.Logger
import ch.bsisa.hyperbird.dao.xqs.XQConnectionHelper
import ch.bsisa.hyperbird.dao.xqs.XQueryHelper
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat

/**
 * Provides CRUD operations for ELFIN
 *
 * @author Patrick Refondini
 */
object ElfinDAO {

  //TODO: full implementation required
  def create()(implicit conf: DbConfig) = {
    val insertStatetement =
      "update insert " + <hb-user name="test1"/> + s" into collection('${conf.databaseName}/security')/hb-users"
    executeStatement(insertStatetement)
  }

  /**
   * Updates XML database ELFIN representation corresponding to the provided ELFIN object.
   * <i>The database API do not provide any feedback on that operation.</i>
   */
  def update(elfin : ELFIN)(implicit conf: DbConfig) = {
    Logger.debug(s"Updating elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id}")
    val elfinXml = ElfinFormat.toXml(elfin)
    val updateStatetement =
      s"update replace collection('${conf.databaseName}/${elfin.ID_G}')//ELFIN[@Id='${elfin.Id}'] with ${elfinXml.mkString}"
    executeStatement(updateStatetement)
  }  

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

  /**
   * designed to executes XUpdate statements
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