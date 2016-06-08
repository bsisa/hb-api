package ch.bsisa.hb.proxy

import play.api.Logger
import play.api.mvc.{ Controller, Action }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import scala.concurrent.Future

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.ApiConfig

/**
 * ProxyController controller. Solves CSRF limitations.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
trait ProxyController extends Controller {

  /**
   * Returns result as JSON
   * @param protocol - Protocol to use to dialog with remote Web Service
   * @param host - Host to use to dialog with remote Web Service
   * @param port - Port to use to dialog with remote Web Service
   */
  def forwardGetTo(requestUrl: String)(protocol: String, host: String, port: String) = Action.async {
    Logger.debug(s"ProxyController.forwardTo(${requestUrl})")
    val wsRespFuture: Future[Response] = WS.url(s"${protocol}://${host}:${port}/${requestUrl}").get()
    wsRespFuture.map { wsResp => Ok(wsResp.json) }
  }
  
  /**
   * Returns result as JSON
   * @param protocol - Protocol to use to dialog with remote Web Service
   * @param host - Host to use to dialog with remote Web Service
   * @param port - Port to use to dialog with remote Web Service
   */
  def forwardPostTo(requestUrl: String)(protocol: String, host: String, port: String) = Action.async(parse.json) { req => 
    Logger.debug(s"ProxyController.forwardPostTo(${requestUrl}) req.body not shown.")
    val wsRespFuture: Future[Response] = WS.url(s"${protocol}://${host}:${port}/${requestUrl}").post(req.body)
    wsRespFuture.map { wsResp => Ok(wsResp.json) }
  }  

}

/**
 * Extend ProxyController trait only to provide hb-geo-api service specific configurations.
 */
object HbGeoProxyController extends ch.bsisa.hb.proxy.ProxyController {

  def getApiConfig()(implicit apiConfig: ApiConfig) = apiConfig

  def forwardGetToHbGeoService(requestUrl: String) = {
    forwardGetTo(requestUrl)(
      protocol = getApiConfig.hbGeoApiProtocol.getOrElse("http"),
      host = getApiConfig.hbGeoApiHost.getOrElse("localhost"),
      port = getApiConfig.hbGeoApiPort.getOrElse("9001"))
  }
  
  def forwardPostToHbGeoService(requestUrl: String) = {
    forwardPostTo(requestUrl)(
      protocol = getApiConfig.hbGeoApiProtocol.getOrElse("http"),
      host = getApiConfig.hbGeoApiHost.getOrElse("localhost"),
      port = getApiConfig.hbGeoApiPort.getOrElse("9001"))
  }  

}