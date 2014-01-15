package ch.bsisa.hyperbird.controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Logger

import scala.concurrent.Future
import scala.xml.XML

import ch.bsisa.hyperbird.dao.TestQueries
import ch.bsisa.hyperbird.dao.XQueryHelper
import ch.bsisa.hyperbird.util.JsonXmlConverter

/**
 * REST API controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
object Api extends Controller {

  /**
   * TODO: review specifications. Listing collections
   */
  def collections = Action {
    // Perform call to eXist REST service to get collections list
    val responseFuture: Future[Response] = WS.url("http://localhost:8080/exist/rest/db/hb").get()

    // Keep asynchronous calls asynchronous to allow Play free threads
    val resultFuture: Future[Result] = responseFuture.map { resp =>
      // We expect to receive XML content
      Logger.debug(s"Result of type ${resp.ahcResponse.getContentType} received")
      // let's convert XML to JSON
      val jsonBody = JsonXmlConverter.xmlStringToJson(resp.body.mkString)
      // Return JSON response
      Status(resp.status)(jsonBody).as(JSON)
    }
    Async(resultFuture)
  }


  def fileteredCollection(collectionId: String, xpath: String) = Action {
    // Perform call to eXist REST service to get collections list
    val responseFuture: Future[Response] = WS.url(s"""http://localhost:8080/exist/rest/db/hb/${collectionId}?_query=${xpath}&_howmany=1000000000""").get()

    // Keep asynchronous calls asynchronous to allow Play free threads
    val resultFuture: Future[Result] = responseFuture.map { resp =>
      // We expect to receive XML content
      Logger.debug(s"Result of type ${resp.ahcResponse.getContentType} received")
      // let's convert XML to JSON
      val jsonBody = JsonXmlConverter.xmlStringToJson(resp.body.mkString)
      // Return JSON response
      Status(resp.status)(jsonBody).as(JSON)
      // Returns original XML response
      //Status(resp.status)(resp.body).as(resp.ahcResponse.getContentType)
    }
    Async(resultFuture)
  }

  def card(collectionId: String, cardId: String) = Action {
    // Perform call to eXist REST service to get collections list
    val responseFuture: Future[Response] = WS.url(s"""http://localhost:8080/exist/rest/db/hb/${collectionId}?_query=//ELFIN%5B@Id=%27${cardId}%27%5D&_howmany=1000000000""").get()

    // Keep asynchronous calls asynchronous to allow Play free threads
    val resultFuture: Future[Result] = responseFuture.map { resp =>
      // We expect to receive XML content
      Logger.debug(s"Result of type ${resp.ahcResponse.getContentType} received")
      // let's convert XML to JSON
      val jsonBody = JsonXmlConverter.xmlStringToJson(resp.body.mkString)
      // Return JSON response
      Status(resp.status)(jsonBody).as(JSON)
      // Returns original XML response
      //Status(resp.status)(resp.body).as(resp.ahcResponse.getContentType)
    }
    Async(resultFuture)
  }

}