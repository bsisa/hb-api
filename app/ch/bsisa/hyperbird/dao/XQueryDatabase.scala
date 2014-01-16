package ch.bsisa.hyperbird.dao

import scala.concurrent.Future
import play.api.mvc.SimpleResult

trait XQueryDatabase {

  def buildQuery(queryName: String, queryParams: String*) : String
  def executeQuery(xquery: String): Future[SimpleResult]
  
}