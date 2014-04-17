package ch.bsisa.hyperbird.controllers

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.ApiConfig
import play.api._
import play.api.mvc._
import controllers.Assets

/**
 * Application controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
object Application extends Controller {

  def getApiBaseUrl()(implicit apiConfig: ApiConfig) =  apiConfig.baseUrl  
  
  /**
   * Added for HTML5 history API support, aka AngularJs html5mode.
   * We need calling AngularJs index from custom controller to preserve full 
   * deep URL while redirecting single page application landing page index.html    
   */
  def angularJsIndex(any: String) = Assets.at(path = "/public", file = "index.html") 

  def index = Action {
    Ok(views.html.index("HyperBird 5.0", ""))
  }
  
  def conf = Action {
    Ok(views.html.conf(getApiBaseUrl)).as("application/javascript; charset=utf-8")
  }
  
}