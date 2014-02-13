package ch.bsisa.hyperbird.dao.ws

import ch.bsisa.hyperbird.dao.{ Queries, DbConfig }
import play.api.Logger
import ch.bsisa.hyperbird.CollectionsConfig

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
   * By default eXist REST API wraps query result within custom XML tags containing result meta-data.
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

  /**
   * Returns a query to access a single ELFIN of CLASSE USER identified by the given email.
   */
  override def elfinUserPerEmailQuery(email: String)(implicit dbConf: DbConfig, collectionsConf: CollectionsConfig): String = {

    // Use store XQuery for complex request
    // Example call:
    // Content-Type: application/xquery
    // http://localhost:8080/exist/rest/db/hb4/queries/ELFIN_USER_find_by_email.xq?email=inaminute@pobox.com
    // The XQuery has access to the entire HTTP context, including parameters and session attributes.

    // ELFIN_USER_find_by_email.xq
    //
    //xquery version "3.0";
    //import module namespace request="http://exist-db.org/xquery/request";
    //let $email := request:get-parameter('email','MISSING EMAIL')
    //let $personne := collection('/db/hb4/')//ELFIN[CARACTERISTIQUE/CAR5/@VALEUR=$email and @CLASSE='ACTEUR' and @TYPE='PERSONNE']
    //let $user := collection('/db/hb4/')//ELFIN[PARTENAIRE/USAGER/@Id=$personne/@Id and @ID_G=$personne/@ID_G]
    //return 
    //(:  USEFUL FOR TESTING   <result email='{$email}'>{$user}</result> :)
    //    $user

    val query = s"""${dbConf.protocol}${dbConf.hostName}:${dbConf.port}${dbConf.restPrefix}${dbConf.databaseName}/${collectionsConf.configurationCollectionId}?_query=//ELFIN%5B@Id=%27${email}%27%5D&_howmany=${highPagingLimit}&_wrap=${wrap}"""
    query
  }

}
