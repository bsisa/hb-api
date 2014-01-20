package ch.bsisa.hyperbird.dao.xqs

import net.xqj.exist.ExistXQDataSource
import ch.bsisa.hyperbird.dao.DbConfig

/**
 * Helper object providing generic javax.xml.xquery.XQConnection
 * hiding database specific data source.
 */
object XQConnectionHelper {

  // TODO: make these properties configurable
  //val host = "localhost"
  //val port = "8080"

  /**
   * @return XQConnection
   */
  def getConnection()(implicit conf: DbConfig) = {
    println("Datasource properties: " + getDataSource(conf.hostName, conf.port.toString()).getSupportedPropertyNames().mkString(";")) 
    getDataSource(conf.hostName, conf.port.toString()).getConnection()
  }

  /**
   * Returns an ExistXQDataSource meant only for use inside the current object.
   *
   * @param serverName
   * @param port
   */
  private def getDataSource(serverName: String, port: String): ExistXQDataSource = {
    val xqs = new ExistXQDataSource()
    xqs.setProperty("serverName", serverName)
    xqs.setProperty("port", port)
    xqs
  }

}