package ch.bsisa.hyperbird.dao

import com.felstar.xqs.XQS
import java.util.Properties

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
    val queryResult = xqpe.executeQuery()
    

    var serialisationProps = new java.util.Properties()
    serialisationProps.setProperty("method", "xml")
    serialisationProps.setProperty("indent", "yes")
    serialisationProps.setProperty("encoding", "ISO8859-1")
    //serialisationProps.setProperty("omit-xml-declartaion", "no")
    
    println("DEBUG ======================================== ")
    println("DEBUG ================= ISO8859-1 ======================= ")
    println("DEBUG ======================================== ")
    //println(queryResult.toString())
    
     while (queryResult.next()) {
       //XQItemType type
       val theType = queryResult.getItemType();
       println("Type: " + theType)
       //println("ATOMIC: " + queryResult.getAtomicValue())
       //println(queryResult.getItemAsString(null))
       println(queryResult.getItemAsString(serialisationProps))
     }
    
    
    println("DEBUG ======================================== ")
    println("DEBUG ======================================== ")
    println("DEBUG ======================================== ")
    val xmls: Seq[scala.xml.Elem] = XQS.toSeqXML(queryResult);
    conn.close();
    xmls
  }
  
}