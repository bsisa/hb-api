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
  def getElfinAnnexFile(elfinID_G : String, elfinId : String, fileName: String)(implicit apiConfig : ApiConfig) : File = {

	val hbAnnexRootFolder : java.io.File = new java.io.File(apiConfig.annexesRootFolder)    
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
   * TODO: implement
   */
  def createElfinAnnexFile(elfinID_G : String, elfinId : String, fileName: String) : Unit = {
    Logger.warn(s"Received createElfinAnnexFile(elfinID_G : ${elfinID_G}, elfinId : ${elfinId}, fileName: ${fileName}) request BUT NO IMPLEMENTATION IS AVAILABLE YET.")
  }  
  
  
  
}

case class AnnexesManagerFileNotFoundException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class AnnexesManagerCannotReadFileException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
