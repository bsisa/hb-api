package ch.bsisa.hyperbird.spreadsheet

import java.io.InputStream
import java.util.Date

import org.apache.poi.ss.usermodel._
import org.apache.poi.ss.util.CellReference
import org.jsoup.Jsoup
import play.api.Logger
import securesocial.core.Identity

import scala.collection.JavaConversions._

/**
  * Encapsulate logic and external libraries dependencies required to produce XLS spreadsheet reports.
  * General supported workflow:
  *
  * - Take a Workbook containing a dataSheet and a parameterSheet
  * - Extract parameterSheet XQuery name and parameters
  * - Obtain XQuery result in HTML format expected to contain a single HTML table
  * - Convert the HTML table to Spreadsheet rows and cells and merge them to the dataSheet
  *
  * @author Patrick Refondini
  */
object SpreadSheetBuilder {

  // ==================================================================
  //                            Constants
  // ==================================================================

  val Col0 = 0
  val Col1 = 1
  val Col2 = 2
  val Row0 = 0
  val Row1 = 1

  val DateCssClassName = "date"
  val NumericCssClassName = "num"

  val ParameterSheetName = "Parametres"

  val AbsRow = true
  val AbsCol = true

  val XQueryFileNameCellRef = new CellReference(ParameterSheetName, Row0, Col1, AbsRow, AbsCol)
  val ResultInsertStartCellRef = new CellReference(ParameterSheetName, Row1, Col1, AbsRow, AbsCol)

  // Hyperbird default date format formatter 
  val sdf = new java.text.SimpleDateFormat("yyyy-MM-dd")

  /**
    * Creates Workbook from provided input stream.
    */
  def getWorkbook(workBookStream: InputStream): Workbook = WorkbookFactory.create(workBookStream)

  /**
    * Get the xquery file name defined in the Workbook `wb`
    */
  def getXQueryFileName(wb: Workbook): String = {
    wb.getSheet(XQueryFileNameCellRef.getSheetName)
      .getRow(XQueryFileNameCellRef.getRow)
      .getCell(XQueryFileNameCellRef.getCol)
      .getRichStringCellValue.getString
  }

  /**
    * Adds user info to Workbook sheets footers.
    */
  def insertWorkBookUserDetails(wb: Workbook, userDetails: Identity): Unit = {

    // dateTimeFormat information could be obtained within userDetails or 
    // obtained thanks to it (user profile, language, locale...)
    val dateTimeFormat = "dd.MM.yyyy HH:mm"
    val dtSdf = new java.text.SimpleDateFormat(dateTimeFormat)

    for (i <- 0 until wb.getNumberOfSheets) {
      val sheet = wb.getSheetAt(i)
      val footer = sheet.getFooter
      val preservedFooterCenterContent = if (footer.getCenter.trim.nonEmpty) " - " + footer.getCenter else ""
      footer.setCenter(userDetails.identityId.userId + " - " + dtSdf.format(new Date()) + preservedFooterCenterContent)
    }
  }

  /**
    * Inserts page x / n at footer right position of all Workbook
    * sheet appending to existing content if any (useful for
    * templates update). x: current page, n: total number of pages.
    *
    * Note: Could not find common org.apache.poi.ss.usermodel way
    * to set page numbers! Indeed common interface:
    * <code>org.apache.poi.ss.usermodel.HeaderFooter</code>
    * does not provide page() and numPages() in POI 3.10-FINAL
    */
  def insertWorkBookPageNumbers(wb: Workbook): Unit = {

    for (i <- 0 until wb.getNumberOfSheets) {
      val sheet = wb.getSheetAt(i)
      val footer = sheet.getFooter

      import org.apache.poi.hssf.usermodel.HeaderFooter
      import org.apache.poi.xssf.usermodel.XSSFWorkbook

      if (wb.isInstanceOf[XSSFWorkbook]) {
        footer.setRight(footer.getRight + " page &P / &N")
      } else {
        footer.setRight(footer.getRight + " page  " + HeaderFooter.page + " / " + HeaderFooter.numPages)
      }
    }
  }

  /**
    * Updates workbook `wb` parameter sheet with `queryString` values.
    */
  def updateParameterWorkBook(wb: Workbook, queryString: Map[String, Seq[String]]): Unit = {

    Logger.debug("SpreadSheetBuilder.updateParameterWorkBook called.")

    val parameterSheet: Sheet = wb.getSheet(XQueryFileNameCellRef.getSheetName)

    // Fill parameters values associated to xquery if any
    for (row: Row <- parameterSheet) {
      for (cell: Cell <- row) {

        val cellRef: CellReference = new CellReference(row.getRowNum, cell.getColumnIndex)
        // Rows 0 and 1 contain XQuery request name and insert result cell position.  
        if (cellRef.getRow > 1 && cellRef.getCol == 0) {
          // Get the parameter name specified in the spread sheet
          val parameterName = cell.getCellType match {
            case Cell.CELL_TYPE_STRING => cell.getRichStringCellValue.getString
            case _ =>
              Logger.error(s"Parameter name should be of string type! found: ${cell.getCellType}") // TODO: throw exception
              s"ERROR - Parameter name should be of string type! found: ${cell.getCellType}"
          }

          // From the query string try to find the parameter value corresponding to the parameter name specified in the spread sheet
          val parameterValue = queryString.get(parameterName) match {
            case Some(value) => value.head
            case None =>
              Logger.error(s"No value found in query string for parameter $parameterName") // TODO: throw exception
              s"ERROR - No value found for parameter $parameterName"
          }

          Logger.debug(s"Found parameter named: $parameterName with value >$parameterValue<")

          // Check the parameter value cell type and convert the matching 
          // query parameter value to the given type to preserve parameter 
          // value cell type while updating its content.
          val paramValueCell = row.getCell(1)
          paramValueCell.getCellType match {
            case Cell.CELL_TYPE_BLANK =>
              Logger.debug(s"Updated BLANK parameter value cell with string = $parameterValue")
              paramValueCell.setCellValue(parameterValue)
            case Cell.CELL_TYPE_STRING =>
              Logger.debug(s"Updated STRING parameter value cell with string = $parameterValue")
              paramValueCell.setCellValue(parameterValue)
            case Cell.CELL_TYPE_NUMERIC =>
              Logger.debug(s"Updated parameter value cell with double = $parameterValue")
              Logger.warn("Request parameter used to set numeric cell. Untested operation, date and numeric conversion need extended support.")
              paramValueCell.setCellValue(parameterValue.toDouble)
            // TODO: date and numeric values need a defined format while passed as request parameter and a corresponding formatter
            //                val format = new java.text.SimpleDateFormat("dd-MM-yyyy")
            //                if (DateUtil.isCellDateFormatted(paramValueCell)) paramValueCell.   paramValueCell.setCellValue(Date.parse(parameterValue)) else paramValueCell.getNumericCellValue()
            // TODO: date and numeric values need a defined format while passed as request parameter and a corresponding formatter
            case Cell.CELL_TYPE_BOOLEAN =>
              Logger.debug(s"Updated parameter value cell with string = $parameterValue")
              Logger.warn("Request parameter used to set boolean cell. Untested operation.")
              paramValueCell.setCellValue(parameterValue)
            case Cell.CELL_TYPE_FORMULA =>
              Logger.debug(s"Parameter value cell NOT updated")
              Logger.error("Request parameter used to set formula cell operation currently not supported.")
            case Cell.CELL_TYPE_ERROR =>
              Logger.warn(s"Parameter value cell of type ERROR NOT updated...")
            case _ =>
              Logger.warn("Unknown Cell type!")
          }

        }
        //    Logger.debug(s"${cellRef.formatAsString()} content: ${cellContent} ")

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
      wb.getSheet(ResultInsertStartCellRef.getSheetName)
        .getRow(ResultInsertStartCellRef.getRow)
        .getCell(ResultInsertStartCellRef.getCol())
        .getRichStringCellValue.getString

    val resultDataStartCellRef = new CellReference(resultDataStartCellRefString)

    //Logger.debug(s"resultDataStartCellRefString = ${resultDataStartCellRefString}, resultDataStartCellRef = ${resultDataStartCellRef}")

    // By convention the resultDataStartCellRef is considered to be on the first sheet
    val dataSheet = wb.getSheetAt(0)

    // Make use of a temporarySheet to perform formulas references adjustments 
    // using row shifting without affecting dataSheet formulas we want to keep
    // references untouched (Header totals for instance).
    val temporarySheet = wb.cloneSheet(0)

    // Get the first row as example
    val templateRow = dataSheet.getRow(resultDataStartCellRef.getRow)
    //Logger.debug(s"templateRow last cell num = ${templateRow.getLastCellNum()}");

    import scala.collection.JavaConversions._

    // Parse report HTML table result as org.jsoup.nodes.Document
    val htmlReportDoc = Jsoup.parse(htmlTable)
    // We expect a single table per document
    val tables = htmlReportDoc.select("table")
    // Check htmlTable structure
    if (tables.size() == 0) throw HtmlTableNotFoundException(s"HTML query result is expected to contain a single table but none was found query ${getXQueryFileName(wb)}")
    else if (tables.size() > 1) throw MoreThanOneHtmlTableFoundException(s"HTML query result is expected to contain a single table but ${tables.size()} were found for query ${getXQueryFileName(wb)}")

    // By convention we expect results to be contained in a single HTML table 
    val dataTable = tables.get(0)
    // Make the iterator a collection to allow repeated traversals
    val dataTableTrCollection = dataTable.select("tr").toIndexedSeq
    // Safe guard limit after which we stop looping in search of formula cell 
    val MAX_COL_WITHOUT_FORMULA = 4

    // ================================================================
    // ==== Deal with formula - START
    // ================================================================

    var rowIdxFormulaPass: Integer = resultDataStartCellRef.getRow()
    var maxCellIdxFormulaPass: Integer = 0

    // Index used to check for last loop
    var currFormulaPassIdx = 0

    // Loop on HTML table result rows
    for (row <- dataTableTrCollection) {

      var cellIdxFormulaPass: Integer = resultDataStartCellRef.getCol.toInt
      var nbColWithoutFormula = 0

      // Always create row at start position, it will be shifted down a position afterward
      val dataRow = temporarySheet.createRow(resultDataStartCellRef.getRow)

      // Loop on HTML table result row columns
      for (cell <- row.select("td")) {
        val currSheetCell = dataRow.createCell(cellIdxFormulaPass)
        currSheetCell.setCellValue("SHOULD NEVER BE VISIBLE :: TO BE DELETED WITH temporarySheet :: " + cell.text)
        cellIdxFormulaPass = cellIdxFormulaPass + 1
      }

      // keep looping while MAX_COL_WITHOUT_FORMULA not reached or last template columns reached
      while (nbColWithoutFormula < MAX_COL_WITHOUT_FORMULA && cellIdxFormulaPass < templateRow.getLastCellNum) {

        val exampleCell = templateRow.getCell(cellIdxFormulaPass)

        // Check if next template column contains a formula
        exampleCell.getCellType match {
          case Cell.CELL_TYPE_FORMULA =>
            val formula = exampleCell.getCellFormula
            if (formula.trim().length() > 0) {
              val currSheetCell = dataRow.createCell(cellIdxFormulaPass)
              currSheetCell.setCellType(exampleCell.getCellType)
              currSheetCell.setCellFormula(exampleCell.getCellFormula)
              currSheetCell.setCellStyle(exampleCell.getCellStyle)
              // Leave nbColWithoutFormula untouched
            } else {
              nbColWithoutFormula = nbColWithoutFormula + 1
            }
          case _ =>
            nbColWithoutFormula = nbColWithoutFormula + 1
        }
        // Go a column forward
        cellIdxFormulaPass = cellIdxFormulaPass + 1
      }

      currFormulaPassIdx = currFormulaPassIdx + 1

      // Do not shift row on last loop
      if (currFormulaPassIdx < dataTableTrCollection.length) {
        // Perform shifting on temporarySheet to preserve dataSheet from side effect on "static" formulas
        temporarySheet.shiftRows(resultDataStartCellRef.getRow, rowIdxFormulaPass, 1)
      }

      if (cellIdxFormulaPass > maxCellIdxFormulaPass) maxCellIdxFormulaPass = cellIdxFormulaPass
      rowIdxFormulaPass = rowIdxFormulaPass + 1

    }

    // ================================================================
    // ==== Deal with formula - END
    // ================================================================    

    // ================================================================
    // ==== Deal with data - START
    // ================================================================

    var rowIdx: Integer = resultDataStartCellRef.getRow()
    var maxCellIdx: Integer = 0

    // Loop on HTML table result rows
    for (row <- dataTableTrCollection) {

      var cellIdx: Integer = resultDataStartCellRef.getCol.toInt
      var nbColWithoutFormula = 0

      // Create row on dataSheet for each HTML table result row
      val dataRow = dataSheet.createRow(rowIdx)

      // Loop on HTML table result row columns
      for (cell <- row.select("td")) {

        val currSheetCell = dataRow.createCell(cellIdx)

        if (!cell.text.isEmpty) {
          // Cell type is defined after td class names.
          // Currently supported names for type specific {"date","num"}
          if (cell.text.startsWith("=")) {
            currSheetCell.setCellFormula(cell.text.substring(1))
          } else {
            cell.className() match {
              case DateCssClassName =>
                currSheetCell.setCellValue(sdf.parse(cell.text))
              case NumericCssClassName =>
                currSheetCell.setCellValue(java.lang.Double.parseDouble(cell.text))
              case _ =>
                currSheetCell.setCellValue(cell.text)
            }
          }
        } else {
          currSheetCell.setCellValue(cell.text)
        }

        // Copy example row cells style
        if (templateRow.getCell(cellIdx) != null) {
          val cellStyle = templateRow.getCell(cellIdx).getCellStyle
          currSheetCell.setCellStyle(cellStyle)
        }

        cellIdx = cellIdx + 1
      }


      // ==============================================================
      // Proceed with formulas columns if any
      // ==============================================================

      // keep doing while MAX_NO_FORMULA_FOUND reached
      while (nbColWithoutFormula < MAX_COL_WITHOUT_FORMULA && cellIdx < templateRow.getLastCellNum) {

        val exampleCell = templateRow.getCell(cellIdx)

        // Check if next template column contains a formula
        exampleCell.getCellType match {
          case Cell.CELL_TYPE_FORMULA =>
            //val fRange = exampleCell.getArrayFormulaRange()            
            //val cachedResult = exampleCell.getCachedFormulaResultType()
            val formula = exampleCell.getCellFormula
            //val style = exampleCell.getCellStyle()
            //Logger.debug(s"Formula found: >${formula}<, cachedResult: >${cachedResult}<")
            //Logger.debug(s"Formula range: \nfirst col: ${fRange.getFirstColumn()}\nlast col : ${fRange.getLastColumn()}\nfirst row: ${fRange.getFirstRow()} \nlast row : ${fRange.getLastRow()} \nnb of cells: ${fRange.getNumberOfCells()}")

            if (formula.trim().length() > 0) {
              //Logger.debug(s"formula.trim().length() = ${formula.trim().length()}")
              val currSheetCell = dataRow.createCell(cellIdx)
              currSheetCell.setCellType(exampleCell.getCellType)
              // Get the formula with correct references from temporarySheet
              val shiftedFormulaFromTemporarySheet = temporarySheet.getRow(currSheetCell.getRowIndex).getCell(currSheetCell.getColumnIndex).getCellFormula
              //Logger.debug(s"shiftedFormulaFromTemporarySheet = ${shiftedFormulaFromTemporarySheet}")
              currSheetCell.setCellFormula(shiftedFormulaFromTemporarySheet)
              currSheetCell.setCellStyle(exampleCell.getCellStyle)
              // Leave nbColWithoutFormula untouched
            } else {
              nbColWithoutFormula = nbColWithoutFormula + 1
            }
          case _ => nbColWithoutFormula = nbColWithoutFormula + 1
        }
        cellIdx = cellIdx + 1
      }

      if (cellIdx > maxCellIdx) maxCellIdx = cellIdx
      rowIdx = rowIdx + 1
    }

    // Get rid of the temporary fake sheet.
    wb.removeSheetAt(wb.getSheetIndex(temporarySheet))

    // ================================================================
    // ==== Deal with data - END
    // ================================================================


    // ================================================================
    // ==== Deal with Print - START
    // ================================================================

    val maxColIdx = maxCellIdx
    val maxRowIdx = rowIdx


    definePrintRange(wb, resultDataStartCellRef, maxColIdx, maxRowIdx)
    //Logger.debug(s"maxColIdx = ${maxColIdx}, maxRowIdx = ${maxRowIdx}")

    // ================================================================
    // ==== Deal with Print - END
    // ================================================================    

    // Disabled autoSizeColumn upon user request. Full fixed layout control on 
    // template is prefered to unpredictable dynamic resize.
    // Note: Text wrap can be defined on template example data row and will be preserved for dynamic data.
    /*
    val firstDataRow = dataSheet.getRow(resultDataStartCellRef.getRow() - 1) // -1 to use data header
    val colDataRange = Range(resultDataStartCellRef.getCol(): Int, firstDataRow.getLastCellNum(): Int, step = 1)
    for (i <- colDataRange) {
      dataSheet.autoSizeColumn(i);
    }
    */

  }

  /**
    * Deal with print range for dataSheet adapting from already existing print range.
    */
  def definePrintRange(wb: Workbook, resultDataStartCellRef: CellReference, maxColIdx: Int, maxRowIdx: Int): Unit = {
    for (i <- 0 until wb.getNumberOfSheets) {
      // Only deal with sheets that do not have an already defined print area
      if (wb.getPrintArea(i) == null) {

        val printRangeStart = "$A$1"
        val printRangeStartCellRef = new CellReference(printRangeStart)

        // Compute resultDataEndCellAbsRef column position
        val endCol = if (printRangeStartCellRef.getCol > resultDataStartCellRef.getCol) {
          maxColIdx - (printRangeStartCellRef.getCol - resultDataStartCellRef.getCol)
        } else {
          maxColIdx + (resultDataStartCellRef.getCol - printRangeStartCellRef.getCol)
        }
        // Define new print range end position
        val resultDataEndCellAbsRef = new CellReference(maxRowIdx - 1, endCol, AbsRow, AbsCol)
        val newPrintRange = printRangeStart + ":" + resultDataEndCellAbsRef.formatAsString()
        wb.setPrintArea(i, newPrintRange)
      }
    }
  }

  /**
    * HSSF and XSSF compatible formulas evaluation.
    */
  def evaluateAllFormulaCells(wb: Workbook): Unit = {
    val evaluator = wb.getCreationHelper.createFormulaEvaluator()
    for (i <- 0 until wb.getNumberOfSheets) {
      val sheet = wb.getSheetAt(i)
      for (row <- sheet) {
        for (cell <- row) {
          if (cell.getCellType == Cell.CELL_TYPE_FORMULA) {
            try {
              evaluator.evaluateFormulaCell(cell)
            } catch {
              case e: Throwable =>
                Logger.warn(s"Problem evaluating formulae for cell row ${cell.getRowIndex}, column ${cell.getColumnIndex} : ${e.getMessage}")
            }
          }
        }
      }
    }
  }

}

/**
  * Exception thrown when the expected HTML table is not found within the HTML query result.
  */
case class HtmlTableNotFoundException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

/**
  * Exception thrown when the HTML result contains more than a single expected HTML table.
  */
case class MoreThanOneHtmlTableFoundException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

