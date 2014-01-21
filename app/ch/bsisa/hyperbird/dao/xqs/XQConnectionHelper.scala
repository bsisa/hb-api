package ch.bsisa.hyperbird.dao.xqs

import ch.bsisa.hyperbird.dao.DbConfig
import net.xqj.exist.ExistXQDataSource
import play.api.Logger

/**
 * Helper object providing generic javax.xml.xquery.XQConnection
 * hiding database specific data source.
 */
object XQConnectionHelper {

  /**
   * @return XQConnection
   */
  def getConnection()(implicit conf: DbConfig) = {
    val xqds = getDataSource(serverName = conf.hostName, port = conf.port.toString(), user = conf.userName, password = conf.password)
    Logger.debug("ExistXQDataSource properties: " + xqds.getSupportedPropertyNames().mkString(";")) 
    xqds.getConnection()
  }

  /**
   * Returns an ExistXQDataSource meant only for use inside the current object.
   *
   * @param serverName
   * @param port
   * @param user
   * @param password
   */
  private def getDataSource(serverName: String, port: String, user: String, password: String): ExistXQDataSource = {
    Logger.debug(s"Creating datasource for server: ${serverName}, port: ${port}, user: ${user}")
    val xqs = new ExistXQDataSource()
    xqs.setProperty("serverName", serverName)
    xqs.setProperty("port", port)
    xqs.setUser(user)
    xqs.setPassword(password)
    xqs
  }

}