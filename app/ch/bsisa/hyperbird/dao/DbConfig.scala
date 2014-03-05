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
  val protocol: String = Play.current.configuration.getString(DbConfig.DbProtocolKey) match {
    case Some(dbProtocol) => dbProtocol
    case None => throw DbConfigException(s"Database configuration ${DbConfig.DbProtocolKey} missing")
  }  
  
  /**
   * Database host name
   */
  val hostName: String = Play.current.configuration.getString(DbConfig.DbHostNameKey) match {
    case Some(dbHostName) => dbHostName
    case None => throw DbConfigException(s"Database configuration ${DbConfig.DbHostNameKey} missing")
  }

  /**
   * Database port
   */
  val port: Integer = Play.current.configuration.getInt(DbConfig.DbPortKey) match {
    case Some(dbPort) => dbPort
    case None => throw DbConfigException(s"Database configuration ${DbConfig.DbPortKey} missing")
  }
  
  /**
   * Database REST service prefix 
   */
  val restPrefix: String = Play.current.configuration.getString(DbConfig.DbRestPrefixKey) match {
    case Some(dbRestPrefix) => dbRestPrefix
    case None => throw DbConfigException(s"Database configuration ${DbConfig.DbRestPrefixKey} missing")
  }
  
  /**
   * Database name
   */
  val databaseName: String = Play.current.configuration.getString(DbConfig.DbNameKey) match {
    case Some(dbName) => dbName
    case None => throw DbConfigException(s"Database configuration ${DbConfig.DbNameKey} missing")
  }
  
  val userName: String = Play.current.configuration.getString(DbConfig.DbUserKey) match {
    case Some(dbUserName) => dbUserName
    case None => throw DbConfigException(s"Database configuration ${DbConfig.DbUserKey} missing")
  }
  
  val password: String = Play.current.configuration.getString(DbConfig.DbPasswordKey) match {
    case Some(dbPassword) => dbPassword
    case None => throw DbConfigException(s"Database configuration ${DbConfig.DbPasswordKey} missing")
  }

}

/**
 * Database configuration exception class
 */
case class DbConfigException(message: String = null, cause: Throwable = null) extends Exception(message, cause)


/**
 * Companion object containing constants
 */
object DbConfig {

  private val DbProtocolKey = "hb.db.protocol"
  private val DbHostNameKey = "hb.db.hostName"
  private val DbPortKey = "hb.db.port"
  private val DbRestPrefixKey = "hb.db.restPrefix"
  private val DbNameKey = "hb.db.name"
  private val DbUserKey = "hb.db.user"
  private val DbPasswordKey = "hb.db.password"

}
