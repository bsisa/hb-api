package ch.bsisa.hyperbird.io

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.ApiConfig

import java.io.File
import play.api.Logger

/**
 * Provides miscellaneous methods to obtain files referred to by `ELFIN/ANNEXE/RENVOI/@LIEN` data structure.
 *
 * @author Patrick Refondini
 */
object AnnexesManager {

  /**
   * Return `java.io.File` corresponding to ELFIN identified by `elfinID_G`, `elfinId` and file name `fileName`.
   */
  def getElfinAnnexFile(elfinID_G: String, elfinId: String, fileName: String)(implicit apiConfig: ApiConfig): File = {

    val hbAnnexRootFolder: java.io.File = new java.io.File(apiConfig.annexesRootFolder)
    val filePath = s"/${elfinID_G}/annexes/${elfinId}/${fileName}"
    val file = new java.io.File(hbAnnexRootFolder, filePath)

    if (!file.exists()) {
      Logger.warn(s"Could not find file ${file.getAbsolutePath()}.")
      throw AnnexesManagerFileNotFoundException(s"Could not find file ${fileName}")
    }
    if (!file.canRead()) {
      Logger.warn(s"Could not read file ${file.getAbsolutePath()}. Check system permissions are set correctly.")
      throw AnnexesManagerCannotReadFileException(s"Could not read file ${fileName}")
    }
    file
  }

  /**
   * Creates an empty file at the expected location given all explicit and implicit information. 
   * Return its reference to routine writing to it.
   */
  def createElfinAnnexFile(elfinID_G: String, elfinId: String, fileName: String, overrideFile: Boolean = true)(implicit apiConfig: ApiConfig): File = {

    val hbAnnexRootFolder: java.io.File = new java.io.File(apiConfig.annexesRootFolder)
    val filePath = s"/${elfinID_G}/annexes/${elfinId}/${fileName}"
    val file = new java.io.File(hbAnnexRootFolder, filePath)
    if (file.exists() && !overrideFile) {
      throw AnnexesManagerFileAlreadyExistException(s"File at ${file.getCanonicalPath()} does already exists.")
    } else {
      // Missing parent directories
      if (!file.getParentFile().exists()) {
        // Create missing parent directories
        if (file.getParentFile().mkdirs()) {
          // Create file
          if (file.createNewFile()) {
            file
          } else {
            // Could not create file
            throw AnnexesManagerCannotCreateFileException(s"Could not create file at path ${file.getCanonicalPath()}")
          }
        } else {
          // Failed to create parent directories
          throw AnnexesManagerCannotCreateFileException(s"Could not create parent dirs for file at path ${file.getCanonicalPath()}")
        }
      } else {
        // Create file
        if (file.createNewFile()) {
          file
        } else {
          // Could not create file
          throw AnnexesManagerCannotCreateFileException(s"Could not create file at path ${file.getCanonicalPath()}")
        }
      }
    }
  }

}

case class AnnexesManagerCannotCreateFileException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class AnnexesManagerFileAlreadyExistException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class AnnexesManagerFileNotFoundException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class AnnexesManagerCannotReadFileException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
