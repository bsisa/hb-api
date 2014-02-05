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
import ch.bsisa.hyperbird.dao.Updates
import ch.bsisa.hyperbird.dao.DbConfig
import ch.bsisa.hyperbird.model.ELFIN
import java.util.Calendar
import java.util.GregorianCalendar
import java.util.Date
import net.liftweb.json.DateFormat
import java.text.SimpleDateFormat
import ch.bsisa.hyperbird.model.format.ElfinFormat
import com.ning.http.client.Realm.AuthScheme
import ch.bsisa.hyperbird.util.ElfinIdGenerator

/**
 * Implements QueriesProcessor for REST service.
 *
 * @author Patrick Refondini
 */
object XQueryWSHelper extends Controller with QueriesProcessor with Updates {

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

  override def delete(elfinID_G: String, elfinId: String)(implicit conf: DbConfig): Unit = ???

  override def replace(elfin: ch.bsisa.hyperbird.model.ELFIN)(implicit conf: DbConfig): Unit = ???



  /**
   *  Draft implementation - might need to return created ELFIN or caller should query expected new ELFIN.
   */
  override def create(elfinID_G: String, elfinCLASSE: String)(implicit conf: DbConfig): Unit = {
    val elfinId = ElfinIdGenerator.getNewElfinId
    val fileName = ElfinIdGenerator.getElfinFileName(elfinId, elfinCLASSE)
    val createStatement = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/${elfinID_G}/${fileName}"""
    val elfinXML = <MELFIN><ELFIN Id={ "'" + elfinId + "'" } CLASSE={ "'" + elfinCLASSE + "'" }></ELFIN></MELFIN>

    Logger.debug("createStatement : " + createStatement)

    // TODO: more investigation to catch authentication failures instead of silently failing.
    val responseFuture: Future[Response] = WS.url(createStatement).
      withAuth(conf.userName, conf.password, AuthScheme.BASIC).put(elfinXML)

  }

}