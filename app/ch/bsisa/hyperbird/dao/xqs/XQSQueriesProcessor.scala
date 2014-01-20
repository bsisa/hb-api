package ch.bsisa.hyperbird.dao.xqs

import play.api.Logger
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future
import ch.bsisa.hyperbird.util.JsonXmlConverter
import ch.bsisa.hyperbird.dao.QueriesProcessor
import ch.bsisa.hyperbird.dao.XQueryHelper

object XQSQueriesProcessor extends Controller with QueriesProcessor {

  /**
   * Implements QueriesProcessor
   */
  def query(query: String): Future[SimpleResult] = {

    // Keep asynchronous calls asynchronous to allow Play free threads
    val resultFuture: Future[SimpleResult] = {
      // Make the XQS call asynchronous
      val futureXmlSeqElem: Future[Seq[scala.xml.Elem]] = scala.concurrent.Future {
        // Perform call to eXist via XQS/XQJ
        XQueryHelper.seqOfElem(query)
      }
      // Extract the XML result from the Future
      for {
        xmlSeqElem <- futureXmlSeqElem
      } yield {
        // Convert the XML result to JSON format
        val jsonSeqElem = JsonXmlConverter.xmlSeqToJson(xmlSeqElem)
        Logger.debug("jsonSeqElem: " + jsonSeqElem)
        // Produce a SimpleResult
        Ok(jsonSeqElem).as(JSON)
      }
    }
    resultFuture
  }

}
 

  
