package ch.bsisa.hyperbird.dao

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import play.api.Logger
import scala.concurrent.Future
import play.api.mvc.SimpleResult

/**
 * Common XQuery database invocation trait. 
 *
 * @author Patrick Refondini
 */
trait QueriesProcessor {

 def query(xquery: String): Future[SimpleResult]

}