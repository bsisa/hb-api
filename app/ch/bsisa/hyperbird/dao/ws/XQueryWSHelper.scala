package ch.bsisa.hyperbird.dao.ws

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.QueriesProcessor
import ch.bsisa.hyperbird.dao.Updates
import ch.bsisa.hyperbird.dao.DbConfig
import ch.bsisa.hyperbird.dao.ResultNotFoundException
import ch.bsisa.hyperbird.dao.ExpectedSingleResultException
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat.ElfinFormatException
import ch.bsisa.hyperbird.model.MELFIN
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.util.ElfinIdGenerator
import ch.bsisa.hyperbird.util.format.JsonXmlConvertException

import com.ning.http.client.Realm.AuthScheme

import java.util.Calendar
import java.util.GregorianCalendar
import java.util.Date
import java.text.SimpleDateFormat

import net.liftweb.json.DateFormat

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
object XQueryWSHelper extends Controller with QueriesProcessor with Updates {

  /**
   * Returns 0 to n `ELFIN` as a JSON array contained in a `Future[SimpleResult]`
   */
  override def query(query: String): Future[SimpleResult] = {

    val elfinsFuture = queryElfins(query)

    // Keep asynchronous calls asynchronous to allow Play free threads
    val simpleResFuture: Future[SimpleResult] = elfinsFuture.map { elfinsResp =>
      try {
        val elfinsJsArray = ElfinFormat.elfinsToJsonArray(elfinsResp)
        Ok(elfinsJsArray)
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
      // elfinsFromXml unwraps ELFINS from the MELFIN element to return a Seq[ELFIN]
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

    proceedWithSingleElfinResponse(responseFuture, query)
  }

  override def delete(elfin: ELFIN)(implicit conf: DbConfig): Unit = {
    val fileName = ElfinIdGenerator.getElfinFileName(elfin)
    val elfinResourceUrl = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/${elfin.ID_G}/${fileName}"""
    Logger.debug("elfinResourceUrl for DELETE : " + elfinResourceUrl)
    // TODO: more investigation to catch basic authentication failures instead of silently failing.
    val responseFuture: Future[Response] = WS.url(elfinResourceUrl).
      withAuth(conf.userName, conf.password, AuthScheme.BASIC).delete
  }

  /**
   *  Currently not implemented. We rely on XQJ/XQS for updates.
   *  @see ch.bsisa.hyperbird.dao.ElfinDAO.update
   *
   *  TODO: implement this method and rely on it for ElfinDAO.update for simplicity.
   *  Not high priority.
   */
  override def replace(elfin: ELFIN)(implicit conf: DbConfig): Unit = ???

  /**
   *  Creates the provided ELFIN in the database providing no feedback on the operation.
   */
  override def create(elfin: ELFIN)(implicit conf: DbConfig): Unit = {
    val fileName = ElfinIdGenerator.getElfinFileName(elfin)
    val elfinResourceUrl = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/${elfin.ID_G}/${fileName}"""
    // Keep consistent with current database state where each 
    // ELFIN element is contained alone in a MELFIN element. 
    val melfinXML = ElfinFormat.toXml(MELFIN(elfin))

    Logger.debug("elfinResourceUrl for PUT : " + elfinResourceUrl)

    // TODO: more investigation to catch basic authentication failures instead of silently failing.
    val responseFuture: Future[Response] = WS.url(elfinResourceUrl).
      withAuth(conf.userName, conf.password, AuthScheme.BASIC).put(melfinXML)

  }

  /**
   * WS specific implementation to query 0 to 1 ELFIN (ELFIN CLASSE='USER') user by its actor email (ELFIN CLASSE='ACTOR').
   */
  def findElfinUserPerEmailQuery(email: String): Future[ELFIN] = {
    // Perform call to eXist REST service to get collections list
    val query = WSQueries.elfinUserPerEmailQuery(email)
    val responseFuture: Future[Response] = WS.url(query).withHeaders(("Content-Type", "application/xquery")).get

    proceedWithSingleElfinResponse(responseFuture, query)
  }

  
  /**
   * Proceeds with response for ELFIN queries expecting a single result.
   */
  private def proceedWithSingleElfinResponse(singleElfinResponseFuture: Future[Response], query : String) : Future[ELFIN] = {
  
    // Keep asynchronous calls asynchronous to allow Play free threads
    val resultFuture: Future[ELFIN] = singleElfinResponseFuture.map { resp =>
      // We expect to receive XML content
      Logger.debug(s"Result of type ${resp.ahcResponse.getContentType} received")
      val bodyString = resp.body.mkString
      if (!(bodyString.length > 0)) {
        throw ResultNotFoundException(s"No ELFIN found for query: ${query}")
      } else {
        // Parse XML (Need to wrap the list of XML elements received to obtain valid XML in case several ELFIN are returned.)
        val melfinElem = scala.xml.XML.loadString(s"<MELFIN>${bodyString}</MELFIN>")
        val elfinNodeSeq = melfinElem \\ "ELFIN"
        if (elfinNodeSeq.size > 1) {
          throw ExpectedSingleResultException(s"Found more than a single ELFIN (${elfinNodeSeq.size}) for query: ${query}")
        } else {
          val elfinElem = elfinNodeSeq(0)
          // Transform XML to ELFIN object
          val elfin = ElfinFormat.fromXml(elfinElem)
          elfin
        }
      }
    }
    resultFuture  
  }
  
  
}