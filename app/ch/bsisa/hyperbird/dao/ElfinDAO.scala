package ch.bsisa.hyperbird.dao

import play.api.Logger
import ch.bsisa.hyperbird.dao.xqs.XQConnectionHelper
import ch.bsisa.hyperbird.dao.xqs.XQueryHelper

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

  //TODO: full implementation required  
  def update(collectionId: String, elfinId: String, elfin: scala.xml.NodeSeq)(implicit conf: DbConfig) = {
    Logger.debug(s"Updating elfin: ${elfin.toString}")
    val insertStatetement =
      s"update replace collection('${conf.databaseName}/${collectionId})//ELFIN[@Id='${elfinId}'] with ${elfin.toString}"
    executeStatement(insertStatetement)
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