package ch.bsisa.hyperbird.controllers

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.ApiConfig

import play.api._
import play.api.mvc._

/**
 * Application controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
object Application extends Controller {

  def getApiBaseUrl()(implicit apiConfig: ApiConfig) =  apiConfig.baseUrl  
  
  
  def angularJsIndex = Action {
    Redirect("/index.html")
  }
  
  def index = Action {
    Ok(views.html.index("HyperBird 5.0", ""))
  }
  
  def conf = Action {
    Ok(views.html.conf(getApiBaseUrl)).as("application/javascript")
  }
  
}