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
   * Database REST service prefix 
   */
  val restPrefix: String = Play.current.configuration.getString("hb.db.restPrefix") match {
    case Some(dbRestPrefix) => dbRestPrefix
    case None => throw DbConfigException("Database configuration hb.db.restPrefix missing")
  }
  
  /**
   * Database name
   */
  val databaseName: String = Play.current.configuration.getString("hb.db.name") match {
    case Some(dbName) => dbName
    case None => throw DbConfigException("Database configuration hb.db.name missing")
  }
  
  val userName: String = Play.current.configuration.getString("hb.db.user") match {
    case Some(dbUserName) => dbUserName
    case None => throw DbConfigException("Database configuration hb.db.user missing")
  }
  
  val password: String = Play.current.configuration.getString("hb.db.password") match {
    case Some(dbPassword) => dbPassword
    case None => throw DbConfigException("Database configuration hb.db.password missing")
  }

}

/**
 * Database configuration exception class
 */
case class DbConfigException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
