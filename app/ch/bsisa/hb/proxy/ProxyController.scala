package ch.bsisa.hb.proxy

import play.api.Logger
import play.api.mvc.{Controller, Action}
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import scala.concurrent.Future

/**
 * ProxyController controller. Solves CSRF limitations.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
object ProxyController extends Controller {

  /**
   * Returns result as JSON
   * @param protocol - Protocol to use to dialog with remote Web Service
   * @param host - Host to use to dialog with remote Web Service
   * @param port - Port to use to dialog with remote Web Service
   */
  def forwardTo(requestUrl: String, protocol: String, host: String, port: String) = Action.async {
    Logger.debug(s"ProxyController.forwardTo(${requestUrl})")
    val wsRespFuture: Future[Response] = WS.url(s"${protocol}://${host}:${port}/${requestUrl}").get()
    wsRespFuture.map { wsResp => Ok( wsResp.json) }    
  }

}