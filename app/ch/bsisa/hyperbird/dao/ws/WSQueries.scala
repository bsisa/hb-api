package ch.bsisa.hyperbird.dao.ws

import ch.bsisa.hyperbird.dao.Queries
import ch.bsisa.hyperbird.dao.DbConfig

import play.api.Logger

/**
 * Implements Queries trait accessing database using its REST API web service.
 *
 *  @author Patrick Refondini
 */
object WSQueries extends Queries {

  /**
   * By default eXist REST API limits the number of results returned at once
   * to a very small inadequate number. The lack of a "no limit" configuration 
   * leads to the following arbitrary high hard coded value.
   */
  val highPagingLimit = 1000000000

  /**
   * Implements Queries
   */
  def allHbCollectionsQuery(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.databaseName}?_howmany=${highPagingLimit}"""
    Logger.debug("allHbCollectionsQuery: " + query)
    query
  }

  /**
   * Implements Queries
   */
  def fileteredCollectionQuery(collectionId: String, xpath: String = "//ELFIN")(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.databaseName}${collectionId}?_query=${xpath}&_howmany=${highPagingLimit}"""
    Logger.debug("fileteredCollectionQuery: " + query)
    query    
  }

  /**
   * Implements Queries
   */
  def cardQuery(collectionId: String, cardId: String)(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.databaseName}${collectionId}?_query=//ELFIN%5B@Id=%27${cardId}%27%5D&_howmany=${highPagingLimit}"""
    Logger.debug("card: " + query)
    query
  }
  
  /**
   * Implements Queries
   */
  def cardQuery(cardId: String)(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.databaseName}?_query=//ELFIN%5B@Id=%27${cardId}%27%5D&_howmany=${highPagingLimit}"""
    Logger.debug("card: " + query)
    query
  }  

}