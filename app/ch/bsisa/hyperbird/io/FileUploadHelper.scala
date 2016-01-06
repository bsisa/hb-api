package ch.bsisa.hyperbird.io

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.ApiConfig

import java.io.File
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.io.BufferedInputStream
import java.io.FileInputStream
import org.apache.commons.io.IOUtils
import java.io.OutputStream
import play.api.Logger
import java.util.Date
import scala.annotation.tailrec

/**
 * Provides methods to process files, chunked files related
 * to file upload process.
 *
 * @author Patrick Refondini
 */
object FileUploadHelper {

  /**
   * Performs the assembly of several binary file chunks back to the original single binary file.
   */
  def putChunksTogether(chunksSourceDirectory: File, resultFile: File, fileIdentifier: String, totalChunks: Int, chunkSize: Int, totalSize: Int): Unit = {

    Logger.debug(s"FileUploadHelper.putChunksTogether()")

    val appendable = true
    val resultFileBOS = new BufferedOutputStream(new FileOutputStream(resultFile, appendable))

    // Check we can read from source folder
    if (!chunksSourceDirectory.canRead()) throw FileUploadHelperCannotReadFileException(s"Cannot read from ${chunksSourceDirectory.getCanonicalPath()}")
    // Check we can write to destination folder
    if (!resultFile.canWrite()) throw FileUploadHelperCannotWriteFileException(s"Cannot write to ${resultFile.getCanonicalPath()}")

    try {
      // Chunks are one based
      appendChunks(chunkNb = 1, chunkSize = chunkSize, totalChunks = totalChunks, chunksSourceFolder = chunksSourceDirectory, fileIdentifier = fileIdentifier, resultFileOS = resultFileBOS)
    } finally {
      IOUtils.closeQuietly(resultFileBOS)
    }

    // Check total obtained file size after chunks assembly has completed equals to expected provided total file size
    if (resultFile.length() != totalSize) {
      throw FileUploadHelperWrongFileSizeException(s"Expected file size ${totalSize} does not match ${resultFile.length()} for file ${resultFile.getCanonicalPath()}")
    }
  }

  /**
   * Delete chunk files
   */
  def deleteChunks(chunksSourceDirectory: File, fileIdentifier: String, totalChunks: Int): Unit = {

    Logger.debug(s"FileUploadHelper.deleteChunks()")

    for (i <- 1 to totalChunks) {
      val chunk = getChunkFile(chunksSourceDirectory, fileIdentifier, chunkNb = i)
      if (chunk.exists()) chunk.delete()
    }
  }

  /**
   * Checks all chunks are available and account for the expected `totalSize`
   * return true if correct false otherwise and also throw any file not found or any other File exception.
   */
  def checkUploadComplete(chunksSourceDirectory: File, fileIdentifier: String, totalChunks: Int, totalSize: Int): Boolean = {

    Logger.debug(s"FileUploadHelper.checkUploadComplete()")

    val files = for { i <- 1 to totalChunks } yield { getChunkFile(chunksSourceDirectory, fileIdentifier, chunkNb = i) }
    val sizes = for { file <- files } yield { file.length() }
    val totalFilesSize = sizes.sum // One liner: val totalFilesSize = files.map{ file => file.length() }.sum
    if (totalSize == totalFilesSize) {
      true
    } else {
      throw FileUploadHelperUploadIncompleteException(s"Upload incomplete for fileIdentifier = ${fileIdentifier}")
    }
  }

  /**
   * Returns a reference to the temporary file upload directory.
   * If this directory does not exist it will be created.
   */
  def getTemporaryFileUploadDirectory()(implicit apiConfig: ApiConfig): File = {
    val temporaryUploadDirectory: java.io.File = new java.io.File(apiConfig.temporaryUploadFolder)
  	// Make sure it exists
    temporaryUploadDirectory.mkdir();
  	temporaryUploadDirectory
  }

  /**
   * Recursively perform chunk assembly to file
   */
  @tailrec
  private def appendChunks(chunkNb: Int, chunkSize: Int, totalChunks: Int, chunksSourceFolder: File, fileIdentifier: String, resultFileOS: OutputStream): Unit = {

    // Create chunk reference
    val chunkFile = getChunkFile(chunksSourceFolder, fileIdentifier, chunkNb)

    // Check chunk exists
    if (!chunkFile.exists()) {
      throw FileUploadHelperMissingFileChunkException(s"Expected file chunk ${chunkFile.getCanonicalPath()} not found.")
    } else {
      // Check whether with reached the last chunk
      if (chunkNb < totalChunks) {
        // Check chunk size is correct
        if (chunkFile.length() == chunkSize) {
          // Perform chunk assembly
          appendChunk(chunkFile, resultFileOS)
          // Recurse for next chunk
          appendChunks(chunkNb + 1, chunkSize, totalChunks, chunksSourceFolder, fileIdentifier, resultFileOS)
        } else {
          throw FileUploadHelperWrongFileChunkSizeException(s"Expected file chunk size ${chunkSize} but found ${chunkFile.getCanonicalPath()} with size ${chunkFile.length()} .")
        }
      } else if (chunkNb == totalChunks) {
        // Perform last chunk assembly
        appendChunk(chunkFile, resultFileOS)
      }
    }
  }

  /**
   * Copy chunk file binary content to result file output stream.
   */
  private def appendChunk(chunkFile: File, resultFileOS: OutputStream): Unit = {
    val currentChunkBIS = new BufferedInputStream(new FileInputStream(chunkFile))
    try {
      IOUtils.copy(currentChunkBIS, resultFileOS)
    } finally {
      IOUtils.closeQuietly(currentChunkBIS)
    }
  }

  private def getChunkFile(chunksSourceFolder: File, fileIdentifier: String, chunkNb: Int): File = {
    new File(chunksSourceFolder, fileIdentifier + "-" + chunkNb)
  }

}

case class FileUploadHelperUploadIncompleteException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

case class FileUploadHelperWrongFileSizeException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class FileUploadHelperWrongFileChunkSizeException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class FileUploadHelperMissingFileChunkException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class FileUploadHelperCannotReadFileException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class FileUploadHelperCannotWriteFileException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

