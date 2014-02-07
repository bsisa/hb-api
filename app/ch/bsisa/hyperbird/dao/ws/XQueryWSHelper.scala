package ch.bsisa.hyperbird.dao.ws

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
import ch.bsisa.hyperbird.model.format.ElfinFormat.ElfinFormatException
import ch.bsisa.hyperbird.model.MELFIN

/**
 * Implements QueriesProcessor for REST service.
 *
 * @author Patrick Refondini
 */
object XQueryWSHelper extends Controller with QueriesProcessor with Updates {

  /**
   * Returns 0 to n `ELFIN` encapsulated in a `MELFIN`, in JSON format
   * itself contained in a `Future[SimpleResult]`
   */
  override def query(query: String): Future[SimpleResult] = {

    val elfinsFuture = queryElfins(query)

    // Keep asynchronous calls asynchronous to allow Play free threads
    val simpleResFuture: Future[SimpleResult] = elfinsFuture.map { elfinsResp =>
      try {
        val elfinsJson = ElfinFormat.elfinsToJson(elfinsResp)
        val melfinJson = ElfinFormat.elfinsJsonToMelfinJson(elfinsJson)
        Ok(melfinJson).as(JSON)
      } catch {
        case e: Throwable =>
          val jsonExceptionMsg = Json.obj(
            "ERROR" -> e.toString(),
            "DESCRIPTION" -> e.getMessage())
          // Returns HTTP error code with valid JSON response containing description of exception 
          // TODO: check this with API users.
          InternalServerError(jsonExceptionMsg).as(JSON)
      }
    }
    simpleResFuture
  }

  /**
   * WS specific implementation to query 0 to n ELFIN
   */
  def queryElfins(query: String): Future[Seq[ELFIN]] = {
    // Perform call to eXist REST service to get collections list
    val responseFuture: Future[Response] = WS.url(query).get()

    // Keep asynchronous calls asynchronous to allow Play free threads
    val resultFuture: Future[Seq[ELFIN]] = responseFuture.map { resp =>
      // We expect to receive XML content
      Logger.debug(s"Result of type ${resp.ahcResponse.getContentType} received")
      // Parse XML (Need to wrap the list of XML elements received to obtain valid XML.)
      val melfinElem = scala.xml.XML.loadString("<MELFIN>" + resp.body.mkString + "</MELFIN>")
      val elfins = ElfinFormat.elfinsFromXml(melfinElem)
      elfins
    }
    resultFuture
  }

  /**
   * WS specific implementation to query 0 to 1 ELFIN.
   */
  def find(query: String): Future[ELFIN] = {
    // Perform call to eXist REST service to get collections list
    val responseFuture: Future[Response] = WS.url(query).get()

    // Keep asynchronous calls asynchronous to allow Play free threads
    val resultFuture: Future[ELFIN] = responseFuture.map { resp =>
      // We expect to receive XML content
      Logger.debug(s"Result of type ${resp.ahcResponse.getContentType} received")
      // Parse XML (Need to wrap the list of XML elements received to obtain valid XML.)
      //val melfinElem = scala.xml.XML.loadString("<MELFIN>" + resp.body.mkString + "</MELFIN>")
      val elfinElem = scala.xml.XML.loadString(resp.body.mkString)
      // Transform XML to ELFIN object
      val elfins = ElfinFormat.fromXml(elfinElem)
      elfins
    }
    resultFuture
  }

  override def delete(elfin: ELFIN)(implicit conf: DbConfig): Unit = ???

  override def replace(elfin: ELFIN)(implicit conf: DbConfig): Unit = ???

  /**
   *  Creates the provided ELFIN in the database providing no feedback on the operation.
   */
  override def create(elfin: ELFIN)(implicit conf: DbConfig): Unit = {
    val fileName = ElfinIdGenerator.getElfinFileName(elfin.Id, elfin.CLASSE)
    val createStatement = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/${elfin.ID_G}/${fileName}"""
    // Keep consistent with current database state where each 
    // ELFIN element is contained alone in a MELFIN element. 
    val melfinXML = ElfinFormat.toXml(MELFIN(elfin))

    Logger.debug("createStatement : " + createStatement)

    // TODO: more investigation to catch basic authentication failures instead of silently failing.
    val responseFuture: Future[Response] = WS.url(createStatement).
      withAuth(conf.userName, conf.password, AuthScheme.BASIC).put(melfinXML)

  }

}