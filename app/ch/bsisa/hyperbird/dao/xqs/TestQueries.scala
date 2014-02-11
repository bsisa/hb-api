package ch.bsisa.hyperbird.dao.xqs

import ch.bsisa.hyperbird.util.format.JsonXmlConverter

/**
 * TODO: to be removed
 * 
 * Predefined XQueries meant for prototyping/testing
 * 
 * @author Patrick Refondini
 * 
 */
object TestQueries {

  
   /** 
  *  Parametrised query string
  *  
  * Excludes 'credit special'
  * IDENTIFIANT/DE must be text as it may contain data such as 2005|2006|2007 
  */
  def noSpecialCreditQueryString(year: Int, owner: String): String = {
    s"""collection("/db/hb4/G20081113902512302")//ELFIN[not(contains(@GROUPE,'dit')) and not(contains(@GROUPE,'cial'))][contains(./IDENTIFIANT/DE,'${year}')][PARTENAIRE/PROPRIETAIRE/@NOM='${owner}']"""
  }
  
  
//  /**
//   * Query parameters
//   */
//  val start: Double = 1
//  val length: Double = 10
//  val year = 2006
//  val yearMinusOne = year - 1
//  val owner = "NE"
  
  // Query current year 
  //val xmls = XQueryHelper.seqOfElem(noSpecialCreditQueryString(year, owner))
  // Query current year minus one
  //val xmlsYearMinusOne = XQueryHelper.seqOfElem(noSpecialCreditQueryString(yearMinusOne, owner))  
  
  def noSpecialCreditAsJson(year: Int, owner: String): String = { 
    val xqueryString = noSpecialCreditQueryString(year, owner)
    val xmlSeqResult = XQueryHelper.seqOfElem(xqueryString)
    val jsonResult = JsonXmlConverter.xmlSeqToJson(xmlSeqResult)
    jsonResult    
  }
  
  
  
}