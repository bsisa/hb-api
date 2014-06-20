package ch.bsisa.hyperbird.controllers

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.ApiConfig
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.model.ELFIN
import play.api._
import play.api.Logger
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits._
import controllers.Assets
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.dao.ws.WSQueries
import securesocial.core.AuthenticationMethod
import scala.concurrent.Await

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS

/**
 * Application controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
object Application extends Controller with securesocial.core.SecureSocial {

  def getApiBaseUrl()(implicit apiConfig: ApiConfig) = apiConfig.baseUrl
  def getClientDebugEnabled()(implicit apiConfig: ApiConfig) = apiConfig.clientDebugEnabled

  /**
   * Added for HTML5 history API support, aka AngularJs html5mode.
   * We need calling AngularJs index from custom controller to preserve full
   * deep URL while redirecting single page application landing page index.html
   */
  def angularJsIndex(any: String) = Assets.at(path = "/public", file = "index.html")

  def index = Action {
    Ok(views.html.index("HyperBird 5.0", ""))
  }

  /**
   * Exposes server side configurations as JavaScript variables for server side single
   * location configuration.
   * SecuredAction(ajaxCall = true)
   */
  def conf = Action {
    Ok(views.html.conf(getApiBaseUrl, getClientDebugEnabled)).as("application/javascript; charset=utf-8")
  }

  /**
   * Provide logged user detailed information about himself. Requires authentication. 
   */
  def whoAmI = SecuredAction.async { request =>
    val futureElfinUser = ElfinDAO.findUser(request.user.identityId.userId)
    futureElfinUser.map { elfinUser =>

      val userDetailsId = elfinUser.PARTENAIRE.get.USAGER.get.Id.get
      val userDetailsID_G = elfinUser.PARTENAIRE.get.USAGER.get.ID_G.get
      val futureUserDetails = XQueryWSHelper.find(WSQueries.elfinQuery(userDetailsID_G, userDetailsId))

      val userDetails = Await.result[ELFIN](futureUserDetails, Duration(8000, MILLISECONDS))
      val userFirstName = userDetails.IDENTIFIANT.get.NOM.get
      val userLastName = userDetails.IDENTIFIANT.get.ALIAS.get
      Ok(views.html.whoami(username = request.user.identityId.userId, name = userLastName, surname = userFirstName)).as("application/javascript; charset=utf-8")
    }.recover {
      case e: Throwable => {
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain user information ${e}"))
      }
    }

  }

}