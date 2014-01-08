package ch.bsisa.hyperbird.controllers

import play.api._
import play.api.mvc._

object Api extends Controller {

  def collections = Action {
    Ok(views.html.index("Collections list should be returned..."))
  }

  def collection(collectionId: String) = Action {
    Ok(views.html.index(s"Collection with id $collectionId should be returned..."))
  }
  
  def fileteredCollection(collectionId: String, xpath: String) = Action {
    Ok(views.html.index(s"Collection with id $collectionId filtered by xpath $xpath should be returned..."))
  }  

  def card(collectionId: String, cardId: String) = Action {
    Ok(views.html.index(s"Card with id $cardId from collection $collectionId should be returned..."))
  }
 
  
}