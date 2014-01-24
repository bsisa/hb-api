package ch.bsisa.hyperbird.dao.ws

import play.api.Logger
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import ch.bsisa.hyperbird.dao.QueriesProcessor

/**
 * Implements QueriesProcessor for REST service.
 * 
 * @author Patrick Refondini
 */
object XQueryWSHelper extends Controller with QueriesProcessor {


  override def query(query: String): Future[SimpleResult] = {
    // Perform call to eXist REST service to get collections list
    val responseFuture: Future[Response] = WS.url(query).get()

    // Keep asynchronous calls asynchronous to allow Play free threads
    val resultFuture: Future[SimpleResult] = responseFuture.map { resp =>
      // We expect to receive XML content
      Logger.debug(s"Result of type ${resp.ahcResponse.getContentType} received")
      // let's convert XML to JSON
      val jsonBody = JsonXmlConverter.xmlStringToJson(resp.body.mkString)
      // Return JSON response
      Status(resp.status)(jsonBody).as(JSON)
    }
    resultFuture
  }

}