package ch.bsisa.hb.proxy

import play.api._
import play.api.Logger
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import controllers.Assets
import securesocial.core.AuthenticationMethod
import scala.concurrent.Future

/**
 * ProxyController controller. Solves CSRF limitations.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
object ProxyController extends Controller with securesocial.core.SecureSocial {

  /**
   * Returns result as JSON
   */
  def forwardTo(requestUrl: String, protocol: String, host: String, port: String) = Action.async {
    Logger.debug(s"ProxyController.forwardTo(${requestUrl})")
    val responseFuture: Future[Response] = WS.url(s"${protocol}://${host}:${port}/${requestUrl}").get()
    responseFuture.map { wsResponse =>
      val responseBodyJs = wsResponse.json
      Ok(responseBodyJs) //.as(JSON)
    }

  }

}