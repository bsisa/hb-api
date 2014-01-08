package ch.bsisa.hyperbird.dao

import net.xqj.exist.ExistXQDataSource // Depends on lib/exist-xqj-x.x.x.jar
import com.felstar.xqs.XQS

/**
 * Helper object providing generic javax.xml.xquery.XQConnection
 * hiding database specific data source.
 */
object XQConnectionHelper {

  // TODO: make these properties configurable
  val host = "localhost"
  val port = "8080"

  /**
   * @return XQConnection
   */
  def getConnection() = getDataSource(host, port).getConnection()

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