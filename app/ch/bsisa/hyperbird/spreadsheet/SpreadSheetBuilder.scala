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
  val ResultInsertStartCellRef = new CellReference(ParameterSheetName, Row1, Col1, AbsRow, AbsCol)

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

  /**
   * Merge `htmlTable` string expected to contain a simple HTML
   * document containing a single HTML table within the provided
   * `wb` Workbook first sheet.
   */
  def mergeHtmlTable(wb: Workbook, htmlTable: String): Unit = {

    Logger.debug("SpreadSheetBuilder.mergeHtmlTable called.")

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
    val htmlReportDoc = Jsoup.parse(htmlTable);
    // We expect a single table per document
    val table = htmlReportDoc.select("table").get(0)

    var rowIdx: Integer = resultDataStartCellRef.getRow()

    for (row <- table.select("tr")) {
      var cellIdx: Integer = resultDataStartCellRef.getCol()
      val dataRow = dataSheet.createRow(rowIdx);
      for (cell <- row.select("td")) {
        dataRow.createCell(cellIdx).setCellValue(cell.text());
        cellIdx = cellIdx + 1
      }
      rowIdx = rowIdx + 1
    }

    // Resize columns to fit their content width
    val firstDataRow = dataSheet.getRow(resultDataStartCellRef.getRow())
    val colDataRange = Range(resultDataStartCellRef.getCol(): Int, firstDataRow.getLastCellNum(): Int, step = 1)
    for (i <- colDataRange) {
      dataSheet.autoSizeColumn(i);
    }

  }

}