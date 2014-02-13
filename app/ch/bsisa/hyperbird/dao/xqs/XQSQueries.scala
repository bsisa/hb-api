package ch.bsisa.hyperbird.dao.xqs

import ch.bsisa.hyperbird.dao.{Queries,DbConfig}
import play.api.Logger
import ch.bsisa.hyperbird.CollectionsConfig

/**
 * Implements Queries trait accessing database using XQS/XQJ drivers.
 *
 *  @author Patrick Refondini
 */
object XQSQueries extends Queries {

  /**
   * Implements Queries
   */
  def allHbCollectionsQuery(implicit conf: DbConfig): String = {
    //TODO: The response produced by xmldb:get-child-collections('${conf.databaseName}') 
    // does not produce valid XML but a list of strings.
    // Review this query.
    // val query = s"""xmldb:get-child-collections('${conf.databaseName}')"""
    // TODO: We could Queries trait could enforce Option[String] as return type 
    // allowing for clean Not implemented messages from the API.
    val query = s""""NOT IMPLEMENTED"""" 
    Logger.debug("allHbCollectionsQuery: " + query)
    query
    
  }

  /**
   * Implements Queries
   */
  def filteredCollectionQuery(collectionId: String, xpath: String = "//ELFIN")(implicit conf: DbConfig): String = {
    val query = s"""collection('${conf.databaseName}/${collectionId}')${xpath}"""
    Logger.debug("fileteredCollectionQuery: " + query)
    query    
  }

  /**
   * Implements Queries
   */
  def elfinQuery(collectionId: String, elfinId: String)(implicit conf: DbConfig): String = {
    val query = s"""collection('${conf.databaseName}/${collectionId}')//ELFIN[@Id='${elfinId}']"""
    Logger.debug("elfin: " + query)
    query
  }
  
  /**
   * Implements Queries
   */
  def elfinQuery(elfinId: String)(implicit conf: DbConfig): String = {
    val query = s"""collection('${conf.databaseName}')//ELFIN[@Id='${elfinId}']"""
    Logger.debug("elfin: " + query)
    query
  }  

  
  /**
   * Returns a query to access a single ELFIN of CLASSE USER identified by the given email.
   * TODO: Not implemented yet.
   */
  def elfinUserPerEmailQuery(email: String)(implicit dbConf: DbConfig, collectionsConf: CollectionsConfig): String = {
    val query = s""""NOT IMPLEMENTED"""" 
    Logger.debug("elfinUserPerEmailQuery: " + query)
    query    
  }  
  
}