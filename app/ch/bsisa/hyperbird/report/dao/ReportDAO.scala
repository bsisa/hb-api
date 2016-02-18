/**
 * Report data access related functions 
 */
package ch.bsisa.hyperbird.report.dao

import java.io.File
import ch.bsisa.hyperbird.dao.ws.{ XQueryWSHelper, WSQueries }
import ch.bsisa.hyperbird.io.AnnexesManager
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.model._

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits._

import scala.concurrent.Future

/**
 * 
 * @author Patrick Refondini
 *
 */
object ReportDAO {

  /**
   * Returns (ID_G, CLASSE, Id)
   */
  def parseTriplet(triplet: String): (String, String, String) = {
    val res = triplet.split("/")
    (res(0), res(1), res(2))
  }

  /**
   * For a given ELFIN identified by its triplet `ID_G/CLASSE/Id` 
   * returns the first ANNEX document whose name ends with .pdf
   */
  def getFirstPdfAnnexe(triplet: String): Future[Option[File]] = {
    val identifier = parseTriplet(triplet)
    val collectionId = identifier._1
    val elfinId = identifier._3

    val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(collectionId, elfinId))
    val futureFileOpt = futureElfin.map { elfin =>
      elfin.ANNEXE.flatMap { anx =>
        val fileNameOpt = anx.RENVOI.find { _.LIEN.toString().endsWith(".pdf") } map { _.LIEN.toString() }
        val fileOpt = fileNameOpt.map(name => AnnexesManager.getElfinAnnexFile(elfinID_G = collectionId, elfinId = elfinId, fileName = name))
        fileOpt
      }
    }
    futureFileOpt
  }

}