package ch.bsisa.hyperbird.spreadsheet

import scala.collection.JavaConversions._

import java.io.InputStream
//import java.io.OutputStream
//import java.io.ByteArrayOutputStream
//import java.io.ByteArrayInputStream

import play.api.Logger

import org.apache.poi.ss.usermodel._
import org.apache.poi.ss.util.CellReference

import org.jsoup.Jsoup

object SpreadSheetBuilder {

  val Col0 = 0
  val Col1 = 1
  val Col2 = 2

  val Row0 = 0
  val Row1 = 1

  val ParameterSheetName = "Parametres"

  val AbsRow = true
  val AbsCol = true

  val XQueryFileNameCellRef = new CellReference(ParameterSheetName, Row0, Col1, AbsRow, AbsCol)

  /**
   * Creates Workbook from provided input stream.
   */
  def getWorkbook(workBookStream: InputStream): Workbook = WorkbookFactory.create(workBookStream)

  /**
   * Get the xquery file name defined in the Workbook `wb`
   */
  def getXQueryFileName(wb: Workbook): String = {
    wb.getSheet(XQueryFileNameCellRef.getSheetName())
      .getRow(XQueryFileNameCellRef.getRow())
      .getCell(XQueryFileNameCellRef.getCol())
      .getRichStringCellValue().getString()
  }

  /**
   * Updates workbook `wb` parameter sheet with `queryString` values.
   */
  def updateParameterWorkBook(wb: Workbook, queryString: Map[String, Seq[String]]): Unit = {

    Logger.debug("SpreadSheetBuilder.updateParameterWorkBook called.")
    
    val parameterSheet: Sheet = wb.getSheet(XQueryFileNameCellRef.getSheetName())

    // Fill parameters values associated to xquery if any
    for (row: Row <- parameterSheet) {
      for (cell: Cell <- row) {

        val cellRef: CellReference = new CellReference(row.getRowNum(), cell.getColumnIndex())
        // Rows 0 and 1 contain XQuery request name and insert result cell position.  
        if (cellRef.getRow() > 1 && cellRef.getCol() == 0) {
          // Get the parameter name specified in the spread sheet
          val parameterName = cell.getCellType() match {
            case Cell.CELL_TYPE_STRING => cell.getRichStringCellValue().getString()
            case _ =>
              Logger.error(s"Parameter name should be of string type! found: ${cell.getCellType}") // TODO: throw exception
              s"ERROR - Parameter name should be of string type! found: ${cell.getCellType}"
          }
          Logger.debug(s"Found parameter named: ${parameterName}")

          // From the query string try to find the parameter value corresponding to the parameter name specified in the spread sheet
          val parameterValue = queryString.get(parameterName) match {
            case Some(value) => value(0)
            case None =>
              Logger.error(s"No value found in query string for parameter ${parameterName}") // TODO: throw exception
              s"ERROR - No value found for parameter ${parameterName}"
          }

          // Check the parameter value cell type and convert the matching 
          // query parameter value to the given type to preserve parameter 
          // value cell type while updating its content.
          val paramValueCell = row.getCell(1)
          paramValueCell.getCellType() match {
            case Cell.CELL_TYPE_STRING => paramValueCell.setCellValue(parameterValue)
            case Cell.CELL_TYPE_NUMERIC =>
              Logger.warn("Request parameter used to set numeric cell. Untested operation, date and numeric conversion need extended support.")
              paramValueCell.setCellValue(parameterValue)
            // TODO: date and numeric values need a defined format while passed as request parameter and a corresponding formatter
            //                val format = new java.text.SimpleDateFormat("dd-MM-yyyy")
            //                if (DateUtil.isCellDateFormatted(paramValueCell)) paramValueCell.   paramValueCell.setCellValue(Date.parse(parameterValue)) else paramValueCell.getNumericCellValue()
            // TODO: date and numeric values need a defined format while passed as request parameter and a corresponding formatter
            case Cell.CELL_TYPE_BOOLEAN =>
              Logger.warn("Request parameter used to set boolean cell. Untested operation.")
              paramValueCell.setCellValue(parameterValue)
            case Cell.CELL_TYPE_FORMULA =>
              Logger.error("Request parameter used to set formula cell operation currently not supported.")
            // TODO: throw exception. Not supported operation ...
            case _ => "Unknown Cell type"
          }

        }

        //          val cellContent = cell.getCellType() match {
        //            case Cell.CELL_TYPE_STRING => cell.getRichStringCellValue().getString()
        //            case Cell.CELL_TYPE_NUMERIC => if (DateUtil.isCellDateFormatted(cell)) cell.getDateCellValue() else cell.getNumericCellValue()
        //            case Cell.CELL_TYPE_BOOLEAN => cell.getBooleanCellValue()
        //            case Cell.CELL_TYPE_FORMULA => cell.getCellFormula()
        //            case _ => "Unknown Cell type"
        //          }
        //
        //          Logger.debug(s"${cellRef.formatAsString()} content: ${cellContent} ")

      }
    }

  }

}