package ch.bsisa.hyperbird.dao

import com.felstar.xqs.XQS

/**
 * Helper object to query XML database using XQuery.
 */
object XQueryHelper {

    /**
   * Encapsulate Exist connection, prepare expression and query execution.
   * Connection seems to be light weight... To be verified.
   */
  def seqOfElem(xquery: String): Seq[scala.xml.Elem] = {

    val conn = XQConnectionHelper.getConnection()
    val xqpe = conn.prepareExpression(xquery)
    val xmls: Seq[scala.xml.Elem] = XQS.toSeqXML(xqpe.executeQuery());
    conn.close();
    xmls
  }
  
}