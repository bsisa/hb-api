package test.ch.bsisa.hyperbird.io

import test.ch.bsisa.hyperbird.util.BaseSerialisationSpec
import ch.bsisa.hyperbird.io.FileUploadHelper
import java.io.File
import org.specs2.mutable._

/**
 * Tests ch.bsisa.hyperbird.io.FileUploadHelper.putChunksTogether() method
 * This method is used to perform the assembly of several binary file chunks
 * back to the original single binary file.
 *
 * Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.io.FileUploadHelperSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class FileUploadHelperSpec extends BaseSerialisationSpec {

  val mergedFile1Name = "MergedFile1.pdf"

  val chunksFilePath = TestResourcesDir
  val fileDestinationPath = TestResultsDir
  val mergedFile1 = new File(new File(TestResultsDir), mergedFile1Name)
  val mergedFile2Size = 10558

  // Clean up result test data if any
  if (mergedFile1.exists()) mergedFile1.delete()

  FileUploadHelper.putChunksTogether(
    chunksFolderSourcePath = chunksFilePath,
    mergedChunksFolderDestinationPath = fileDestinationPath,
    fileName = mergedFile1Name,
    fileIdentifier = "10558-MergedFile1pdf",
    totalChunks = 10,
    chunkSize = 1024,
    totalSize = mergedFile2Size)

  s"The file chunks in ${chunksFilePath} " should {
    s"correspond to checkUploadComplete" in {
      FileUploadHelper.checkUploadComplete(
        chunksFolderSourcePath = chunksFilePath,
        fileIdentifier = "10558-MergedFile1pdf",
        totalChunks = 10,
        totalSize = mergedFile2Size)
    }
  }

  s"The mergedFile1 at ${mergedFile1.getCanonicalPath()} " should {
    s"exist" in {
      mergedFile1.exists()
    }
    s"have size equal to " in {
      mergedFile1.length() === mergedFile2Size
    }
  }

}