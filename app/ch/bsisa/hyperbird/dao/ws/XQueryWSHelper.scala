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
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.model.proto._
import play.api.libs.json.Json
import java.io.FileWriter
import play.api.libs.json.JsError
import play.api.libs.json.JsObject

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
      // Transform XML to JSON 
      val melfinJs = elfinsXmlToJson(melfinElem)
      // Return JSON response
      Status(resp.status)(melfinJs).as(JSON)
    }
    resultFuture
  }

  /**
   * Transforms a scala.xml.NodeSeq expected to contain
   * a MELFIN containing a sequence of ELFIN elements to
   * a Json object containing an array of ELFIN Json objects.
   */
  def elfinsXmlToJson(melfinElem: scala.xml.Elem): JsObject = {

    // Unwrap dummy wrap tag
    val elfinNodeSeq = melfinElem \\ "ELFIN"

    // Convert XML to scala objects
    val elfins = for { elfinNode <- elfinNodeSeq } yield scalaxb.fromXML[ELFIN](elfinNode)
    Logger.debug("elfins objects nb: " + elfins.size)

    // Convert Scala objects to JSON
    val elfinsJson = for { elfin <- elfins } yield //Json.toJson(elfin)
    {
      try {
        Json.toJson(elfin)
      } catch { // No need to be more specific as long as we cannot return distinct information.
        case exception: Throwable =>
          Logger.debug(s"${exception} with elfin: ${elfin.Id}")
          Json.obj(
            "ERROR" -> s"ELFIN with ID_G: ${elfin.ID_G} and Id: ${elfin.Id} failed to serialise to JSON",
            "DESCRIPTION" -> exception.toString())
      }
    }
    Json.obj("MELFIN" -> elfinsJson)
  }

}