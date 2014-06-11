package ch.bsisa.hyperbird.controllers

import play.api._
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.mvc._
import scala.concurrent.Future
import scala.xml.XML
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.util.format.JsonXmlConverter
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.dao.xqs.XQSQueriesProcessor
import ch.bsisa.hyperbird.dao.xqs.XQSQueries
import ch.bsisa.hyperbird.dao.ElfinDAO
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.util.ElfinIdGenerator
import ch.bsisa.hyperbird.model.ELFIN
import play.api.libs.json.Json
import ch.bsisa.hyperbird.util.ElfinUtil
import ch.bsisa.hyperbird.InitConfig
import ch.bsisa.hyperbird.CollectionsConfig
import securesocial.core.java.SecureSocial.SecuredAction
import ch.bsisa.hyperbird.dao.ResultNotFoundException
import java.net.ConnectException
import java.io.InputStream
import play.api.libs.iteratee.Enumerator
import org.apache.poi.ss.usermodel._
import java.io.OutputStream
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import org.apache.poi.ss.util.CellReference
import scala.concurrent.Await
import org.jsoup.Jsoup

import ch.bsisa.hyperbird.spreadsheet.SpreadSheetBuilder


/**
 * Database migration controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 */
object DbMigration extends Controller with securesocial.core.SecureSocial {

  def migrate() = SecuredAction {
    Logger.debug("Running db migration...")
    ch.bsisa.hyperbird.db.evolution.Version4To5.fontainesStructUpdate()
    Ok(views.html.index("HyperBird 5.0", "Check Version4To5.scala for details"))
  }

}