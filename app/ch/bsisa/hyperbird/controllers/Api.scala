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

//import org.apache.poi.ss.usermodel.{WorkbookFactory,Workbook,Sheet,Row,Cell}

/**
 * REST API controller.
 *
 * Check conf/routes file for URLs to functions mapping.
 *
 * @author Patrick Refondini
 * @author Guy de Pourtales
 */
object Api extends Controller with securesocial.core.SecureSocial {

  val JsonFormat = "json"
  val OriginalFormat = "original"

  /**
   * Helper function obtaining configuration information.
   */
  private def getConfigJson()(implicit initConfig: InitConfig, collectionsConfig: CollectionsConfig): String = {
    val configJson =
      s"""{"config": { 
    "hb_init_ref": { 
      "Id": "${initConfig.initElfinId}",
      "ID_G": "${collectionsConfig.configurationCollectionId}"
    }
  }
}"""
    configJson
  }

  /**
   * Dynamically provides initialisation configuration information useful to hb-ui
   */
  def config() = SecuredAction(ajaxCall = true) {
    Ok(getConfigJson).as(JSON)
  }

  /**
   * TODO: review specifications. Listing collections.
   * <ul>
   * <li>Do we really have any use case for this ?</li>
   * <li>What type of data to return in geoXml.xsd world ?</li>
   * </ul>
   */
  def collections = SecuredAction(ajaxCall = true).async {
    XQueryWSHelper.query(WSQueries.allHbCollectionsQuery)
  }

  /**
   * Returns the result of executing the specified XQuery file by name.
   *
   * Supported `format` parameter value are `{original|json}`
   */
  def runXQueryFile(xqueryFileName: String, format: String) = SecuredAction(ajaxCall = true).async { request =>
    val queryString = if (request.rawQueryString != null && request.rawQueryString.nonEmpty) Option(request.rawQueryString) else None
    Logger.debug(s"Run XQuery ${xqueryFileName} with returned format = ${format} and rawQueryString: ${queryString}")

    XQueryWSHelper.runXQueryFile(xqueryFileName, queryString).map { response =>
      format match {
        case JsonFormat =>
          val melfinWrappedBody = "<MELFIN>" + response.body.mkString + "</MELFIN>"
          Ok(ElfinFormat.elfinsToJsonArray(ElfinFormat.elfinsFromXml(scala.xml.XML.loadString(melfinWrappedBody))))
        case OriginalFormat => Ok(response.body).as(response.ahcResponse.getContentType())
      }
    }
  }

  /**
   * Returns the file identified by fileName in binary format.
   */
  def getFile(fileName: String) = SecuredAction(ajaxCall = true).async { request =>

    val queryString = if (request.rawQueryString != null && request.rawQueryString.nonEmpty) Option(request.rawQueryString) else None

    
    XQueryWSHelper.getFile(fileName).map { response =>
      val asStream: InputStream = response.ahcResponse.getResponseBodyAsStream
      // Status(response.status)(response.body).as(response.ahcResponse.getContentType)
      //Logger.debug(s"getFile ${fileName} content type: ${response.ahcResponse.getContentType}")
      Logger.debug(s"getFile ${fileName} content type: ${response.ahcResponse.getContentType} and rawQueryString: ${queryString}")

      // Test reading and modifying XLS document using Apache POI.
      // TODO: move this logic out of here to a POI specific component.

      Logger.debug(s"Modifying ${fileName} content using Apache POI !!!")

      val Col0 = 0
      val Col1 = 1
      val Col2 = 2

      val Row0 = 0
      val Row1 = 1

      val ParameterSheetName = "Parametres"

      val AbsRow = true
      val AbsCol = true

      //val XQueryFileNameCellRef = new CellReference(ParameterSheetName, Row0, Col1, AbsRow, AbsCol)
      val ResultInsertStartCellRef = new CellReference(ParameterSheetName,Row1, Col1, AbsRow, AbsCol)
      
      val wb: Workbook = SpreadSheetBuilder.getWorkbook(asStream)
      val xqueryFileName = SpreadSheetBuilder.getXQueryFileName(wb)

      
      
      // Get the result of the query as an HTML table
      val reportDynamicContentFuture = XQueryWSHelper.runXQueryFile(xqueryFileName, queryString).map { response =>
        response.body.mkString
      }

      import scala.concurrent.duration._
      
      // Reporting may take long! 
      // TODO: check whether a non blocking solution is possible given 
      // reportDynamicContentFuture is dependent on xqueryFileName
      val reportDynamicContent = Await.result(reportDynamicContentFuture,10 minutes)       
      
      Logger.debug("reportDynamicContent: " + reportDynamicContent)
      
      
      
//      val parameterSheet: Sheet = wb.getSheet(XQueryFileNameCellRef.getSheetName()) 
//      val xqueryFileName =
//        wb.getSheet(XQueryFileNameCellRef.getSheetName())
//          .getRow(XQueryFileNameCellRef.getRow())
//          .getCell(XQueryFileNameCellRef.getCol())
//          .getRichStringCellValue().getString()

      val resultDataStartCellRefString = 
        wb.getSheet(ResultInsertStartCellRef.getSheetName())
          .getRow(ResultInsertStartCellRef.getRow())
          .getCell(ResultInsertStartCellRef.getCol())
          .getRichStringCellValue().getString()      
          
      val resultDataStartCellRef = new CellReference(resultDataStartCellRefString)      

      Logger.debug(s"resultDataStartCellRefString = ${resultDataStartCellRefString}, resultDataStartCellRef = ${resultDataStartCellRef}")      
      
      // By convention the resultDataStartCellRef is considered to be on the first sheet
      val dataSheet = wb.getSheetAt(0)
      // Get the first row as example
      val templateRow = dataSheet.getRow(resultDataStartCellRef.getRow())
      

          
      import scala.collection.JavaConversions._
      
      // Parse report HTML table result as org.jsoup.nodes.Document
      val htmlReportDoc  = Jsoup.parse(reportDynamicContent);
      // We expect a single table per document
      val table  = htmlReportDoc.select("table").get(0)

      var rowIdx : Integer = resultDataStartCellRef.getRow()
      
      
      for ( row <- table.select("tr") ) {
    	  var cellIdx : Integer = resultDataStartCellRef.getCol()
    	  val dataRow = dataSheet.createRow(rowIdx);
    	  for ( cell <- row.select("td")) {
    		  dataRow.createCell(cellIdx).setCellValue(cell.text());  
    		  cellIdx = cellIdx + 1  
    	  }
    	  rowIdx = rowIdx + 1
      }
      
      // Resize columns to fit their content width
      val firstDataRow = dataSheet.getRow(resultDataStartCellRef.getRow())
      val colDataRange = Range(resultDataStartCellRef.getCol(): Int, firstDataRow.getLastCellNum() : Int, step = 1) 
      for (i <- colDataRange) {
        dataSheet.autoSizeColumn(i);
      }
     
      
      val queryStringMap = request.queryString

      // Fill parameters values associated to xquery if any
      SpreadSheetBuilder.updateParameterWorkBook(wb, queryStringMap)
      
//      // Fill parameters values associated to xquery if any
//      for (row: Row <- parameterSheet) {
//        for (cell: Cell <- row) {
//
//          val cellRef: CellReference = new CellReference(row.getRowNum(), cell.getColumnIndex())
//          // Rows 0 and 1 contain XQuery request name and insert result cell position.  
//          if (cellRef.getRow() > 1 && cellRef.getCol() == 0) {
//            // Get the parameter name specified in the spread sheet
//            val parameterName = cell.getCellType() match {
//              case Cell.CELL_TYPE_STRING => cell.getRichStringCellValue().getString()
//              case _ =>
//                Logger.error(s"Parameter name should be of string type! found: ${cell.getCellType}") // TODO: throw exception
//                s"ERROR - Parameter name should be of string type! found: ${cell.getCellType}"
//            }
//            Logger.debug(s"Found parameter named: ${parameterName}")
//
//            // From the query string try to find the parameter value corresponding to the parameter name specified in the spread sheet
//            val parameterValue = request.queryString.get(parameterName) match {
//              case Some(value) => value(0)
//              case None =>
//                Logger.error(s"No value found in query string for parameter ${parameterName}") // TODO: throw exception
//                s"ERROR - No value found for parameter ${parameterName}"
//            }
//
//            // Check the parameter value cell type and convert the matching 
//            // query parameter value to the given type to preserve parameter 
//            // value cell type while updating its content.
//            val paramValueCell = row.getCell(1)
//            paramValueCell.getCellType() match {
//              case Cell.CELL_TYPE_STRING => paramValueCell.setCellValue(parameterValue)
//              case Cell.CELL_TYPE_NUMERIC =>
//                Logger.warn("Request parameter used to set numeric cell. Untested operation, date and numeric conversion need extended support.")
//                paramValueCell.setCellValue(parameterValue)
//              // TODO: date and numeric values need a defined format while passed as request parameter and a corresponding formatter
//              //                val format = new java.text.SimpleDateFormat("dd-MM-yyyy")
//              //                if (DateUtil.isCellDateFormatted(paramValueCell)) paramValueCell.   paramValueCell.setCellValue(Date.parse(parameterValue)) else paramValueCell.getNumericCellValue()
//              // TODO: date and numeric values need a defined format while passed as request parameter and a corresponding formatter
//              case Cell.CELL_TYPE_BOOLEAN =>
//                Logger.warn("Request parameter used to set boolean cell. Untested operation.")
//                paramValueCell.setCellValue(parameterValue)
//              case Cell.CELL_TYPE_FORMULA =>
//                Logger.error("Request parameter used to set formula cell operation currently not supported.")
//              // TODO: throw exception. Not supported operation ...
//              case _ => "Unknown Cell type"
//            }
//
//          }
//
//          //          val cellContent = cell.getCellType() match {
//          //            case Cell.CELL_TYPE_STRING => cell.getRichStringCellValue().getString()
//          //            case Cell.CELL_TYPE_NUMERIC => if (DateUtil.isCellDateFormatted(cell)) cell.getDateCellValue() else cell.getNumericCellValue()
//          //            case Cell.CELL_TYPE_BOOLEAN => cell.getBooleanCellValue()
//          //            case Cell.CELL_TYPE_FORMULA => cell.getCellFormula()
//          //            case _ => "Unknown Cell type"
//          //          }
//          //
//          //          Logger.debug(s"${cellRef.formatAsString()} content: ${cellContent} ")
//
//        }
//      }

      val out: ByteArrayOutputStream = new ByteArrayOutputStream()
      wb.write(out)
      out.close()
      val modifiedStream = new ByteArrayInputStream(out.toByteArray)

      Logger.debug(s"Modified ${fileName} according to query = ${xqueryFileName} content sent...")

      //Ok.chunked(Enumerator.fromStream(asStream)).as(response.ahcResponse.getContentType)
      Ok.chunked(Enumerator.fromStream(modifiedStream)).as(response.ahcResponse.getContentType)
    }.recover {
      case e: Throwable => {
        manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain file: ${fileName}: ${e}"))
      }
    }
  }

  /**
   * Returns the list of elfins contained in the specified collection matching the xpath filter expression with defined format
   *
   * TODO: make use of format parameter value, currently returns JSON format only. (format=(json|xml|pdf|xls|...)
   */
  def filteredCollection(collectionId: String, xpath: String, format: String) = SecuredAction(ajaxCall = true).async {
    Logger.warn(s"TODO: make use of format parameter value ${format}")
    XQueryWSHelper.query(WSQueries.filteredCollectionQuery(collectionId, xpath))
  }

  /**
   * Gets new ELFIN instance from catalogue for provided CLASSE. This instance does not exist in database yet.
   */
  def getNewElfin(classeName: String) = SecuredAction(ajaxCall = true).async {

    val futureElfinWithId: Future[ELFIN] = ElfinDAO.getNewFromCatalogue(classeName)

    // Send cloned catalogue elfin in JSON format 
    futureElfinWithId.map { elfin =>
      val elfinJson = ElfinFormat.toJson(elfin)
      Ok(elfinJson).as(JSON)
    }.recover {
      case resNotFound: ResultNotFoundException => {
        manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"Failed to obtain new ELFIN from catalogue for classeName: ${classeName}: ${resNotFound}"))
      }
      case connectException: ConnectException => {
        manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
      }
      case e: Throwable => {
        manageException(exception = Option(e), errorMsg = Option(s"Failed to obtain new ELFIN from catalogue for classeName: ${classeName}: ${e}"))
      }
    }
  }

  /**
   * Gets ELFIN corresponding to this collectionId and elfinId
   */
  def getElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true).async { implicit request =>

    Logger.debug(s"getElfin(collectionId=${collectionId}, elfinId=${elfinId}) called by user: ${request.user}")

    val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(collectionId, elfinId))

    futureElfin.map { elfin =>
      val elfinJson = ElfinFormat.toJson(elfin)
      Ok(elfinJson).as(JSON)
    }.recover {
      case resNotFound: ResultNotFoundException => {
        manageResutlNotFoundException(exception = resNotFound, errorMsg = Option(s"No elfin found for ID_G: ${collectionId}, Id: ${elfinId}"))
      }
      case connectException: ConnectException => {
        manageConnectException(exception = connectException, errorMsg = Option(s"No database connection could be established."))
      }
      case e: Throwable => {
        manageException(exception = Option(e), errorMsg = Option(s"Failed to perform find operation for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"))
      }
    }
  }

  /**
   * Creates an ELFIN within the specified collectionId of CLASS className.
   */
  def createElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true).async(parse.json) { request =>

    try {
      // Convert elfin JsValue to ELFIN object and replace its ID_G with collectionId
      val elfin = ElfinUtil.replaceElfinID_G(elfin = ElfinFormat.fromJson(request.body), newElfinID_G = collectionId)

      // Test identifiers consistency between URL and JSON body
      if (elfin.Id.equals(elfinId)) {
        // Update database with new elfin
        ElfinDAO.create(elfin)
        // Re-query the new ELFIN from the database as the only way we currently 
        // have to detect creation failure due to failing access rights or other issues.
        XQueryWSHelper.query(WSQueries.elfinQuery(collectionId, elfinId))
      } else {
        val errorMsg = s"PUT URL ELFIN ID_G/Id: ${collectionId}/${elfinId} unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Creation cancelled."
        manageFutureException(errorMsg = Option(errorMsg))
      }
    } catch {
      case e: Throwable =>
        manageFutureException(exception = Option(e), errorMsg = Option(s"Failed to perform creation for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"))
    }

  }

  /**
   * Updates ELFIN within the specified collectionId with Id elfinId.
   * The data used to update this ELFIN will only be accepted if provided in JSON format.
   */
  def updateElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true)(parse.json) { request =>
    try {
      // Convert elfin JsValue to ELFIN object
      val elfin = ElfinFormat.fromJson(request.body)

      // Test identifiers consistency between URL and JSON body
      if (elfin.ID_G.equals(collectionId) && elfin.Id.equals(elfinId)) {
        // Update database with new elfin
        ElfinDAO.update(elfin)
        // Sent success response
        //Ok(s"""{"message": "elfin.ID_G/Id: ${elfin.ID_G}/${elfin.Id} update successful"}""").as(JSON)
        Ok(ElfinFormat.toJson(elfin)).as(JSON)
      } else {
        val errorMsg = s"PUT URL ELFIN ID_G/Id: ${collectionId}/${elfinId} unique identifier does not match PUT body JSON ELFIN provided ID_G/Id: ${elfin.ID_G}/${elfin.Id}. Update cancelled."
        manageException(errorMsg = Option(errorMsg))
      }
    } catch {
      case e: Throwable =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        manageException(exception = Option(e), errorMsg = Option(errorMsg))
    }
  }

  /**
   * Deletes an ELFIN within the specified collectionId with Id elfinId.
   * RFC are not cristal clear regarding HTTP DELETE body usage or not but REST
   * principles states that URL information should uniquely identify a resource
   * to GET or DELETE.
   *
   * For that reason we do not process the request body for DELETE operation.
   * In addition trying to process it would fail with REST client such as
   * Restangular which does not sent any body for DELETE operations.
   */
  def deleteElfin(collectionId: String, elfinId: String) = SecuredAction(ajaxCall = true).async { request =>
    try {
      // Info level is fine for DELETE operation. They should not be frequent and should be easily traceable.
      Logger.info(s"deleteElfin(collectionId=${collectionId}, elfinId=${elfinId}) called by user: ${request.user}")
      // Make sure the resource we want to delete still exists.
      val futureElfin = XQueryWSHelper.find(WSQueries.elfinQuery(collectionId, elfinId))
      futureElfin.map(elfin =>
        try {
          // Delete elfin from database
          ElfinDAO.delete(elfin)
          // Send deleted elfin back to give a chance for cancellation (re-creation) 
          // provided the REST client does something with it unlike restangular
          Ok(ElfinFormat.toJson(elfin)).as(JSON)
        } catch {
          case e: Throwable =>
            manageException(exception = Option(e), errorMsg = Option(s"Failed to perform find operation for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"))
        })
    } catch {
      case e: Throwable =>
        val errorMsg = s"Failed to perform update for Elfin with ID_G: ${collectionId}, Id: ${elfinId}: ${e}"
        manageFutureException(exception = Option(e), errorMsg = Option(errorMsg))
    }
  }

  // 566 - Custom code for connect exception
  def manageConnectException(exception: ConnectException, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> "db.connection.failure",
      "DESCRIPTION" -> errorMsg.getOrElse("").toString)
    Status(566)(jsonExceptionMsg)
  }

  /**
   * Encapsulate `manageConnectException` in a asynchronous call for use in Action.async context.
   * @see manageException
   */
  def manageFutureConnectException(exception: ConnectException, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageConnectException(exception, errorMsg) }

  def manageResutlNotFoundException(exception: ResultNotFoundException, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> "no.result.found",
      "DESCRIPTION" -> errorMsg.getOrElse("").toString)
    NotFound(jsonExceptionMsg)
  }

  /**
   * Encapsulate `manageException` in a asynchronous call for use in Action.async context.
   * @see manageException
   */
  def manageFutureResutlNotFoundException(exception: ResultNotFoundException, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageResutlNotFoundException(exception, errorMsg) }

  /**
   * Utility method to return exception, error message in a generic JSON error message.
   */
  def manageException(exception: Option[Throwable] = None, errorMsg: Option[String] = None): SimpleResult = {
    Logger.warn("Api exception: " + exception.getOrElse("").toString + " - " + errorMsg.getOrElse(""))
    val jsonExceptionMsg = Json.obj(
      "ERROR" -> exception.getOrElse("application.validation.failure").toString,
      "DESCRIPTION" -> errorMsg.getOrElse(exception.getOrElse("None").toString).toString // TODO: review
      )
    InternalServerError(jsonExceptionMsg)
  }

  /**
   * Encapsulate `manageException` in a asynchronous call for use in Action.async context.
   * @see manageException
   */
  def manageFutureException(exception: Option[Throwable] = None, errorMsg: Option[String] = None): Future[SimpleResult] =
    scala.concurrent.Future { manageException(exception, errorMsg) }

}
