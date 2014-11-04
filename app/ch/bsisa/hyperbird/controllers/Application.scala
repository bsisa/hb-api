package ch.bsisa.hyperbird.controllers

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.ApiConfig
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.model.format.Implicits._
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
import ch.bsisa.hyperbird.security.Role
import ch.bsisa.hyperbird.util.DateUtil
import java.util.Date

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
   * WARN: for developement only. Comment out otherwise.
   * Exposed defaultFooter with no data for quick layout prototyping, by passing login, database querying... 
   *   */
//  def footer = Action {
//    Ok(views.html.reports.defaultFooter(<ELFIN></ELFIN>))
//  }  

  /**
   * Provide logged user detailed information about himself. Requires authentication. 
   */
  def whoAmI = SecuredAction(ajaxCall = true).async { request =>
    
    val futureElfinUser = ElfinDAO.findUser(request.user.identityId.userId)
    val elfinUser = Await.result[ELFIN](futureElfinUser, Duration(8000, MILLISECONDS))
    futureElfinUser.map { elfinUser =>

      val userDetailsId = elfinUser.PARTENAIRE.get.USAGER.get.Id.get
      val userDetailsID_G = elfinUser.PARTENAIRE.get.USAGER.get.ID_G.get
      val futureUserDetails = XQueryWSHelper.find(WSQueries.elfinQuery(userDetailsID_G, userDetailsId))

      val userDetails = Await.result[ELFIN](futureUserDetails, Duration(8000, MILLISECONDS))

      val userFirstName = userDetails.IDENTIFIANT.get.NOM.get
      val userLastName = userDetails.IDENTIFIANT.get.ALIAS.get      
      val userAbbreviation = userDetails.GROUPE.get

      // We can assume a user always has a FRACTION
      val userRoles = for {
        line <- elfinUser.CARACTERISTIQUE.get.FRACTION.get.L
      } yield {
        val cSeq = line.C.seq
        Role(ID_G = getMixedContent(cSeq(0).mixed), Id = getMixedContent(cSeq(1).mixed), name = getMixedContent(cSeq(2).mixed))        
      }
      
      Ok(views.txt.whoami(username = request.user.identityId.userId, name = userLastName, surname = userFirstName, abbreviation = userAbbreviation,  roles = userRoles)).as(JSON)
    }.recover {
      case e: Throwable => {
        ExceptionsManager.manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain user information ${e}"))
      }
    }

  }

}