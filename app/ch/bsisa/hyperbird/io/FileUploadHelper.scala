package ch.bsisa.hyperbird.io

import java.io.File
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.io.BufferedInputStream
import java.io.FileInputStream
import org.apache.commons.io.IOUtils
import java.io.OutputStream
import play.api.Logger

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

    try {
      // Chunks are one based
      for (i <- 1 to totalChunks) {
        val chunkFile = new File(chunksSourceFolder, fileIdentifier + "-" + i)
        appendChunk(chunkFile, resultFileBOS)
      }
    } finally {
      IOUtils.closeQuietly(resultFileBOS)
    }
  }

  private def appendChunk(chunkFile: File, resultFileOS: OutputStream): Unit = {
    val currentChunkBIS = new BufferedInputStream(new FileInputStream(chunkFile))
    try {
      IOUtils.copy(currentChunkBIS, resultFileOS)
    } finally {
      IOUtils.closeQuietly(currentChunkBIS)
    }

  }

}
