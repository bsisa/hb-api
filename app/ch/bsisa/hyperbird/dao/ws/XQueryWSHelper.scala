package ch.bsisa.hyperbird.dao.ws

import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import ch.bsisa.hyperbird.dao.QueriesProcessor
import ch.bsisa.hyperbird.util.format.JsonXmlConvertException
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future

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
      // Parse XML (Need to wrap the list of XML elements received to obtain valid XML.)
      val melfinElem = scala.xml.XML.loadString("<MELFIN>" + resp.body.mkString + "</MELFIN>")
      try {
        // Transform XML to JSON 
        val melfinJs = JsonXmlConverter.elfinsXmlToJson(melfinElem)
        // Return valid JSON response
        Status(resp.status)(melfinJs).as(JSON)
      } catch {
        case jxce: JsonXmlConvertException =>
          val jsonExceptionMsg = Json.obj(
            "ERROR" -> jxce.getMessage(),
            "DESCRIPTION" -> jxce.getCause().toString())
          // Return valid JSON response containing description of exception 
          // TODO: It might be better to return an HTTP error code check this with API users.
          Status(resp.status)(jsonExceptionMsg).as(JSON)
      }
    }
    resultFuture
  }

}