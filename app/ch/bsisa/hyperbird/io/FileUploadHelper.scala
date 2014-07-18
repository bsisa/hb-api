package ch.bsisa.hyperbird.io

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
  def putChunksTogether(chunksFolderSourcePath: String, mergedChunksFolderDestinationPath: String, fileName: String, fileIdentifier: String, totalChunks: Int, chunkSize: Int, totalSize: Int): Unit = {

    val chunksSourceFolder = new File(chunksFolderSourcePath)
    val fileDestinationFolder = new File(mergedChunksFolderDestinationPath)
    val resultFile = new File(fileDestinationFolder, fileName)
    val appendable = true
    val resultFileBOS = new BufferedOutputStream(new FileOutputStream(resultFile, appendable))

    // Check we can read from source folder
    if (!chunksSourceFolder.canRead()) throw FileUploadHelperCannotReadFileException(s"Cannot read from ${chunksSourceFolder.getCanonicalPath()}")
    // Check we can write to destination folder
    if (!fileDestinationFolder.canWrite()) throw FileUploadHelperCannotWriteFileException(s"Cannot write to ${fileDestinationFolder.getCanonicalPath()}")

    try {
      // Chunks are one based
      appendChunks(chunkNb = 1, chunkSize = chunkSize, totalChunks = totalChunks, chunksSourceFolder = chunksSourceFolder, fileIdentifier = fileIdentifier, resultFileOS = resultFileBOS)
    } finally {
      IOUtils.closeQuietly(resultFileBOS)
    }

    // Check total obtained file size after chunks assembly has completed equals to expected provided total file size
    if (resultFile.length() != totalSize) {
      throw FileUploadHelperWrongFileSizeException(s"Expected file size ${totalSize} does not match ${resultFile.length()} for file ${resultFile.getCanonicalPath()}")
    }
  }

  /**
   * Recursively perform chunk assembly to file
   */
  @tailrec
  private def appendChunks(chunkNb: Int, chunkSize: Int, totalChunks: Int, chunksSourceFolder: File, fileIdentifier: String, resultFileOS: OutputStream): Unit = {

    // Create chunk reference
    val chunkFile = new File(chunksSourceFolder, fileIdentifier + "-" + chunkNb)

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

}

case class FileUploadHelperWrongFileSizeException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class FileUploadHelperWrongFileChunkSizeException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class FileUploadHelperMissingFileChunkException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class FileUploadHelperCannotReadFileException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
case class FileUploadHelperCannotWriteFileException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

