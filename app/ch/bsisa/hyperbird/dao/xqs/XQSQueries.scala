package ch.bsisa.hyperbird.dao.xqs

import ch.bsisa.hyperbird.dao.{Queries,DbConfig}

import play.api.Logger

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
    //TODO: Check this effectively lists collections from XQJ/XQS
    val query = s"""xmldb:get-child-collections('${conf.databaseName}')"""
    Logger.debug("allHbCollectionsQuery: " + query)
    query
  }

  /**
   * Implements Queries
   */
  def fileteredCollectionQuery(collectionId: String, xpath: String = "//ELFIN")(implicit conf: DbConfig): String = {
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

  
}