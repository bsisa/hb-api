package ch.bsisa.hyperbird.dao

import play.api.Logger
import play.api.Play

/**
 * Database configurations object
 *
 * @author Patrick Refondini
 */
class DbConfig {

  
  /**
   * Database protocol
   */
  val protocol: String = Play.current.configuration.getString("hb.db.protocol") match {
    case Some(dbProtocol) => dbProtocol
    case None => throw DbConfigException("Database configuration hb.db.protocol missing")
  }  
  
  /**
   * Database host name
   */
  val hostName: String = Play.current.configuration.getString("hb.db.hostName") match {
    case Some(dbHostName) => dbHostName
    case None => throw DbConfigException("Database configuration hb.db.hostName missing")
  }

  /**
   * Database port
   */
  val port: Integer = Play.current.configuration.getInt("hb.db.port") match {
    case Some(dbPort) => dbPort
    case None => throw DbConfigException("Database configuration hb.db.port missing")
  }
  /**
   * Database name
   */
  val databaseName: String = Play.current.configuration.getString("hb.db.name") match {
    case Some(dbName) => dbName
    case None => throw DbConfigException("Database configuration hb.db.name missing")
  }

}

/**
 * Database configuration exception class
 */
case class DbConfigException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
