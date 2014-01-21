package ch.bsisa.hyperbird.dao.ws

import ch.bsisa.hyperbird.dao.{ Queries, DbConfig }

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
   * By default eXist REST API wraps query result within custom XML tags 
   * containing result meta-data.
   */
  val wrap = "no"

  /**
   * Implements Queries
   */
  def allHbCollectionsQuery(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/?_howmany=${highPagingLimit}&_wrap=${wrap}"""
    Logger.debug("allHbCollectionsQuery: " + query)
    query
  }

  /**
   * Implements Queries
   */
  def filteredCollectionQuery(collectionId: String, xpath: String = "//ELFIN")(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/${collectionId}?_query=${xpath}&_howmany=${highPagingLimit}&_wrap=${wrap}"""
    Logger.debug("fileteredCollectionQuery: " + query)
    query
  }

  /**
   * Implements Queries
   */
  def elfinQuery(collectionId: String, elfinId: String)(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/${collectionId}?_query=//ELFIN%5B@Id=%27${elfinId}%27%5D&_howmany=${highPagingLimit}&_wrap=${wrap}"""
    Logger.debug("elfin: " + query)
    query
  }

  /**
   * Implements Queries
   */
  def elfinQuery(elfinId: String)(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/?_query=//ELFIN%5B@Id=%27${elfinId}%27%5D&_howmany=${highPagingLimit}&_wrap=${wrap}"""
    Logger.debug("elfin: " + query)
    query
  }

}
