package ch.bsisa.hyperbird.controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def angularJsIndex = Action {
    Redirect("/index.html")
  }
  
  def index = Action {
    Ok(views.html.index("HyperBird 5.0", ""))
  }
  
  def conf = Action {
    Ok(views.html.conf("http://www.789.ch:2199/api/melfin/")).as("application/javascript")
    //.as("application/javascript")
//
  }


}