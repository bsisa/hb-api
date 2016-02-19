/**
 * Report data access related functions
 */
package ch.bsisa.hyperbird.report.dao

import java.io.File
import ch.bsisa.hyperbird.dao.ws.{ XQueryWSHelper, WSQueries }
import ch.bsisa.hyperbird.io.AnnexesManager
import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.model._
import ch.bsisa.hyperbird.model.format.Implicits.getMixedContent

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

  /**
   * Returns `(filesToMergeBefore,filesToMergeAfter)` as `Future[Option[(Seq[File], Seq[File])]]`
   */
  def getPdfAnnexFilesToMerge(elfinId: String, elfinID_G: String): Future[Option[(Seq[File], Seq[File])]] = {
    val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(elfinID_G, elfinId))
    val futureFileOpt = futureElfin.map { elfin =>
      val res = elfin.ANNEXE.map { anx =>
        val filesToMergeBefore = getPdfFilesRefForTag("action::merge::before", anx.RENVOI, elfinID_G, elfinId)
        val filesToMergeAfter = getPdfFilesRefForTag("action::merge::after", anx.RENVOI, elfinID_G, elfinId)
        (filesToMergeBefore, filesToMergeAfter)
      }
      res
    }
    futureFileOpt
  }

  def getPdfAnnexPathsToMerge(elfinId: String, elfinID_G: String): Future[Option[(Seq[String], Seq[String])]] = {
    val futureFilesOptToMerge = getPdfAnnexFilesToMerge(elfinId, elfinID_G)
    val futureFilepathsOptToMerge = futureFilesOptToMerge map {
      futureFilesToMerge =>
        futureFilesToMerge map {
          filesToMerge =>
            val filesPathsToMergeBefore = filesToMerge._1.map(file => file.getCanonicalPath)
            val filesPathsToMergeAfter = filesToMerge._2.map(file => file.getCanonicalPath)
            (filesPathsToMergeBefore, filesPathsToMergeAfter)
        }
    }
    futureFilepathsOptToMerge
  }

  def getPdfFilesRefForTag(tag: String, renvois: Seq[RENVOI], elfinID_G: String, elfinId: String): Seq[File] = {

    val filesToMergeForTagRef = renvois.filter {
      r => (r.LIEN.toString.endsWith(".pdf") && getMixedContent(r.mixed).contains(tag))
    }
    val filesToMergeForTag = for { fileRef <- filesToMergeForTagRef } yield {
      val fileName = fileRef.LIEN.toString() // TODO: Legacy data might require further parsing. Check UI processing.
      val file = AnnexesManager.getElfinAnnexFile(elfinID_G = elfinID_G, elfinId = elfinId, fileName = fileName)
      file
    }
    filesToMergeForTag
  }

}