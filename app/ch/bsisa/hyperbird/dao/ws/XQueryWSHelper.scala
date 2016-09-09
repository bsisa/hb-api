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
import ch.bsisa.hyperbird.controllers.Api
import play.api.cache.Cache
import play.api.Play.current
import play.api.cache.EhCachePlugin
import ch.bsisa.hyperbird.cache.CacheHelper
import ch.bsisa.hyperbird.CollectionsConfig

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

    // Check cache 
    val cachedQueryResultOption = CacheHelper.getCachedJsonValue(query)
    cachedQueryResultOption match {
      case Some(cachedQueryResult) => {
        Logger.debug(s">>>> CACHE <<<< : Using cache for query ${query}")
        // Return result from cache as a Future to satisfy function signature
        scala.concurrent.Future { Ok(cachedQueryResult) }
      }
      case None => {
        queryDb(query)
      }

    }

  }

  /**
   * Perform query to database managing cache
   */
  private def queryDb(query: String) = {
    // Keep asynchronous calls asynchronous to allow Play free threads
    val simpleResFuture: Future[SimpleResult] = queryElfins(query).map { elfinsResp =>

      val elfinsJsArray = ElfinFormat.elfinsToJsonArray(elfinsResp)

      // Manage query cache
      CacheHelper.setCache(key = query, value = elfinsJsArray)

      Ok(elfinsJsArray)
    }.recover {
      case e: ElfinFormatException => Api.manageElfinFormatException(e, Some("ELFIN format conversion failed."))
      case e: NumberFormatException =>
        Logger.error(s">>>> NumberFormatException while proceeding with queryDb: query ${query} ")
        val jsonExceptionMsg = Json.obj(
          "ERROR" -> e.toString(),
          "DESCRIPTION" -> "Could not format number successfully.")
        InternalServerError(jsonExceptionMsg).as(JSON)
      case e: Throwable =>
        val jsonExceptionMsg = Json.obj(
          "ERROR" -> e.toString(),
          "DESCRIPTION" -> e.getMessage())
        InternalServerError(jsonExceptionMsg).as(JSON)
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
      Logger.debug(s"QueryElfins: Result of type ${resp.ahcResponse.getContentType} received")
      // Parse XML (Need to wrap the list of XML elements received to obtain valid XML.)
      val melfinElem = scala.xml.XML.loadString("<MELFIN>" + resp.body.mkString + "</MELFIN>")
      Logger.debug("About to unwraps ELFINS from the MELFIN element to return a Seq[ELFIN]")
      // elfinsFromXml unwraps ELFINS from the MELFIN element to return a Seq[ELFIN]
      val elfins = ElfinFormat.elfinsFromXml(melfinElem)
      Logger.debug(s"ELFINS successfully unwrapped from the MELFIN element and returned a Seq[ELFIN] with ${elfins.size} elements.")
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

    proceedWithSingleElfinResponseToElfinClass(responseFuture, query)
  }

  /**
   * WS specific implementation to query 0 to 1 ELFIN and return original XML format.
   *
   * This is outside the 'QueriesProcessor' trait.
   * Indeed the API is designed around Scala classes not specific JSON, XML,... formats.
   * The purpose of the current function is to avoid XML to class back to XML conversions
   * for performances considerations.
   */
  def findXml(query: String): Future[scala.xml.Node] = {
    // Perform call to eXist REST service to get collections list
    val responseFuture: Future[Response] = WS.url(query).get()

    proceedWithSingleElfinResponseToXML(responseFuture, query)
  }

  override def delete(elfin: ELFIN)(implicit conf: DbConfig): Unit = {
    val fileNameOpt = ElfinIdGenerator.getElfinFileName(elfin)
    fileNameOpt match {
      case Some(fileName) =>
        val elfinResourceUrl = s"""${conf.protocol}${conf.hostName}:${conf.port}${conf.restPrefix}${conf.databaseName}/${elfin.ID_G}/${fileName}"""
        Logger.debug("elfinResourceUrl for DELETE : " + elfinResourceUrl)

        // Invalidate all cache entries related to this collectionId (All queries containing this elfin)
        CacheHelper.removeEntriesContaining(elfin.ID_G)

        // TODO: more investigation to catch basic authentication failures instead of silently failing.
        val responseFuture: Future[Response] = WS.url(elfinResourceUrl).
          withAuth(conf.userName, conf.password, AuthScheme.BASIC).delete
      case None =>
        Logger.error(s"Cannot delete ELFIN with Id, ID_G, CLASSE : ${elfin.Id} ${elfin.ID_G} ${elfin.CLASSE}")
    }

  }

  /**
   *  Currently not implemented. We rely on XQJ/XQS for updates.
   *  @see ch.bsisa.hyperbird.dao.ElfinDAO.update
   *
   *  TODO: implement this method and rely on it for ElfinDAO.update for simplicity.
   *  Low priority.
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
    val melfinXML = ElfinFormat.toXml(MELFIN(Seq(elfin)))

    //Logger.debug("elfinResourceUrl for PUT : " + elfinResourceUrl)

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

    proceedWithSingleElfinResponseToElfinClass(responseFuture, query)
  }

  /**
   * Build query given `xqueryFileName` and executes it returning a future response.
   * TODO: Review explicit specification of implicit parameters (unnecessary, see runWrappedXQueryFile)
   */
  //def runXQueryFile(xqueryFileName: String, queryString: Option[String])(implicit dbConf: DbConfig, collectionsConf: CollectionsConfig): Future[Response] = {
  //val query = WSQueries.runXQueryFile(xqueryFileName, queryString)(dbConf, collectionsConf)
  def runXQueryFile(xqueryFileName: String, queryString: Option[String]): Future[Response] = {
    val query = WSQueries.runXQueryFile(xqueryFileName, queryString)
    executeQuery(query)
    //     val responseFuture: Future[Response] = WS.url(query).withAuth(dbConf.userName, dbConf.password, AuthScheme.BASIC).withHeaders(("Content-Type", "application/xquery")).get
    //     responseFuture     
  }

  /**
   * Executes `query` using `userName` and `password` parameters provided by implicit DbConfig for authentication.
   */
  private def executeQuery(query: String)(implicit dbConf: DbConfig): Future[Response] = {
    val responseFuture: Future[Response] = WS.url(query).withAuth(dbConf.userName, dbConf.password, AuthScheme.BASIC).withHeaders(("Content-Type", "application/xquery")).get
    responseFuture
  }

  /**
   * Build query given `xqueryFileName` and executes it returning a future response.
   * TODO: Should call runXQueryFile with additional optional `wrapResult` parameter set to `true`
   */
  def runWrappedXQueryFile(xqueryFileName: String, queryString: Option[String]): Future[Response] = {
    val query = WSQueries.runWrappedXQueryFile(xqueryFileName, queryString)
    val responseFuture: Future[Response] = WS.url(query).withHeaders(("Content-Type", "application/xquery")).get
    responseFuture
  }

  /**
   * Build query given `fileName` and executes it returning a future response.
   *
   * `fileName` must be an XQuery file located in `CollectionsConfig.xqueriesCollectionId` collection
   */
  def getFile(fileName: String): Future[Response] = {
    val query = WSQueries.runXQueryFile(xqueryFileName = fileName, queryString = None)
    val responseFuture: Future[Response] = WS.url(query).withHeaders(("Content-Type", "application/xquery")).get
    // Keep working with default. Kept as syntax reminder.
    //val responseFuture: Future[Response] = WS.url(query).withHeaders(("Content-Type", "application/xquery"),("charset","UTF-8")).get
    responseFuture
  }

  /**
   * Proceeds validating response expected to contain a single result for ELFIN 'query'.
   * The response body must contain a single ELFIN in XML format.
   *
   * Returns a 'Future[scala.xml.Node]' where Node is an ELFIN element if valid.
   * Otherwise throws ResultNotFoundException, ExpectedSingleResultException
   *
   */
  private def proceedWithSingleElfinResponseToXML(singleElfinResponseFuture: Future[Response], query: String): Future[scala.xml.Node] = {

    // Keep asynchronous calls asynchronous to allow Play free threads
    val resultFuture: Future[scala.xml.Node] = singleElfinResponseFuture.map { resp =>
      // We expect to receive XML content
      //Logger.debug(s">>>> proceedWithSingleElfinResponse: Result of type ${resp.ahcResponse.getContentType} received")
      val bodyString = resp.body.mkString
      if (!(bodyString.length > 0)) {
        throw ResultNotFoundException(s"No ELFIN found for query: ${query}")
      } else {
        // Parse XML (Need to wrap the list of XML elements received to obtain valid XML in case several ELFIN are returned.)
        val melfinElem = scala.xml.XML.loadString(s"<MELFIN>${bodyString}</MELFIN>")
        val elfinNodeSeq = melfinElem \\ "ELFIN"
        if (elfinNodeSeq.size == 0) {
          throw ResultNotFoundException(s"No ELFIN found for query: ${query}")
        } else if (elfinNodeSeq.size > 1) {
          throw ExpectedSingleResultException(s"Found more than a single ELFIN (${elfinNodeSeq.size}) for query: ${query}")
        } else {
          val elfinElem = elfinNodeSeq(0)
          elfinElem
        }
      }
    }
    resultFuture
  }

  /**
   * Wraps 'proceedWithSingleElfinResponseToXML' to convert ELFIN in XML format to JSON format.
   */
  private def proceedWithSingleElfinResponseToElfinClass(singleElfinResponseFuture: Future[Response], query: String): Future[ELFIN] = {

    val elfinXmlElemFuture = proceedWithSingleElfinResponseToXML(singleElfinResponseFuture, query)
    val elfinClassFuture = elfinXmlElemFuture.map { elfinElem => ElfinFormat.fromXml(elfinElem) }
    elfinClassFuture

  }

}