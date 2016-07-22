package ch.bsisa.hyperbird.dao.ws

import ch.bsisa.hyperbird.dao.{ Queries, DbConfig }
import play.api.Logger
import ch.bsisa.hyperbird.CollectionsConfig
import ch.bsisa.hyperbird.util.UrlEncode

/**
 * Implements Queries trait accessing database using its REST API web service.
 *
 *  @author Patrick Refondini
 */
object WSQueries extends Queries {

  /**
   * By default eXist REST API limits the number of results returned at once
   * to a very small inadequate number (10). The lack of a "no limit" configuration
   * leads to the following arbitrary high hard coded value.
   */
  val highPagingLimit = 1000000000
  /**
   * By default eXist REST API wraps query result within custom XML tags containing result meta-data.
   */
  val wrap = "no"

  /**
   * Implements Queries
   */
  override def allHbCollectionsQuery(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/?_howmany=${highPagingLimit}&_wrap=${wrap}"""
    Logger.debug("allHbCollectionsQuery: " + query)
    query
  }

  /**
   * Implements Queries
   */
  override def filteredCollectionQuery(collectionId: String, xpath: String = "//ELFIN")(implicit conf: DbConfig): String = {
    val encodedXpath = UrlEncode.encodeURLQueryParameter(queryParameter = xpath)
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/${collectionId}?_query=${encodedXpath}&_howmany=${highPagingLimit}&_wrap=${wrap}"""
    Logger.debug(s"fileteredCollectionQuery: ${query} built with provided xpath filter: ${xpath}")
    query
  }

  /**
   * Returns all database ELFIN matching `xpath` parameter, using paging defined by `startIndex` and `maxResults` parameters.
   */
  def filteredGlobalQuery(xpath: String = "//ELFIN", startIndex: Int = 1, maxResult: Int = highPagingLimit)(implicit conf: DbConfig): String = {
    val encodedXpath = UrlEncode.encodeURLQueryParameter(queryParameter = xpath)
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}?_query=${encodedXpath}&_howmany=${maxResult}&_start=${startIndex}&_wrap=${wrap}"""
    Logger.debug(s"filteredCollectionQuery: ${query} built with provided xpath filter: ${xpath}")
    query
  }

  /**
   * Implements Queries
   */
  override def elfinQuery(collectionId: String, elfinId: String)(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/${collectionId}?_query=//ELFIN%5B@Id=%27${elfinId}%27%5D&_howmany=${highPagingLimit}&_wrap=${wrap}"""
    Logger.debug("elfin: " + query)
    query
  }

  /**
   * Implements Queries
   */
  override def elfinQuery(elfinId: String)(implicit conf: DbConfig): String = {
    val query = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/?_query=//ELFIN%5B@Id=%27${elfinId}%27%5D&_howmany=${highPagingLimit}&_wrap=${wrap}"""
    Logger.debug("elfin: " + query)
    query
  }

  /**
   * Returns a query to access a single ELFIN of CLASSE USER identified by the given email.
   */
  override def elfinUserPerEmailQuery(email: String)(implicit dbConf: DbConfig, collectionsConf: CollectionsConfig): String = {
    // Use stored XQuery for complex request
    // Example call:
    // Content-Type: application/xquery
    // http://localhost:8080/exist/rest/db/hb4/queries/ELFIN_USER_find_by_email.xq?email=inaminute@pobox.com
    // TODO: have something generic like elfinForXQuery(parameter*) where parameter are tuples (name,value) ...
    // The XQuery has access to the entire HTTP context, including parameters and session attributes.
    val xqueryResourceName = "ELFIN_USER_find_by_email.xq"
    val query = s"""${dbConf.protocol}${dbConf.hostName}:${dbConf.port}${dbConf.restPrefix}${dbConf.databaseName}/${collectionsConf.xqueriesCollectionId}/${xqueryResourceName}?email=${email}&_howmany=${highPagingLimit}&_wrap=${wrap}"""

    query
  }

  def elfinUserPerNameQuery(name: String)(implicit dbConf: DbConfig, collectionsConf: CollectionsConfig): String = {
    val xqueryResourceName = "ELFIN_USER_find_by_name.xq"
    val query = s"""${dbConf.protocol}${dbConf.hostName}:${dbConf.port}${dbConf.restPrefix}${dbConf.databaseName}/${collectionsConf.xqueriesCollectionId}/${xqueryResourceName}?email=${name}&_howmany=${highPagingLimit}&_wrap=${wrap}"""
    query
  }

  /**
   * Returns a query to execute a given xquery file by name
   * TODO: Add additional optional `wrapResult` parameter set to `false` by default and deprecate or delete runWrappedXQueryFile noise
   */
  def runXQueryFile(xqueryFileName: String, queryString: Option[String])(implicit dbConf: DbConfig, collectionsConf: CollectionsConfig): String = {
    val baseQuery = s"""${dbConf.protocol}${dbConf.hostName}:${dbConf.port}${dbConf.restPrefix}${dbConf.databaseName}/${collectionsConf.xqueriesCollectionId}/${xqueryFileName}?_howmany=${highPagingLimit}&_wrap=${wrap}"""

    val query = queryString match {
      case Some(queryString) => baseQuery + s"&${queryString}"
      case None              => baseQuery
    }
    Logger.debug(s"runXQueryFile query = ${query}")
    query
  }

  /**
   * Returns a query to execute a given xquery file by name
   * TODO: Deprecate or delete in favour of runXQueryFile with additional `wrapResult` parameter
   */
  def runWrappedXQueryFile(xqueryFileName: String, queryString: Option[String])(implicit dbConf: DbConfig, collectionsConf: CollectionsConfig): String = {
    val baseQuery = s"""${dbConf.protocol}${dbConf.hostName}:${dbConf.port}${dbConf.restPrefix}${dbConf.databaseName}/${collectionsConf.xqueriesCollectionId}/${xqueryFileName}?_howmany=${highPagingLimit}&_wrap=yes"""

    val query = queryString match {
      case Some(queryString) => baseQuery + s"&${queryString}"
      case None              => baseQuery
    }
    query
  }

  def getFile(fileName: String)(implicit dbConf: DbConfig, collectionsConf: CollectionsConfig): String = {
    val query = s"""${dbConf.protocol}${dbConf.hostName}:${dbConf.port}${dbConf.restPrefix}${dbConf.databaseName}/${collectionsConf.xqueriesCollectionId}/${fileName}?_howmany=${highPagingLimit}&_wrap=${wrap}"""
    query
  }

}
