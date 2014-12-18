package ch.bsisa.hyperbird.spreadsheet

import scala.collection.JavaConversions._
import java.io.InputStream
import play.api.Logger
import securesocial.core.Identity
import org.apache.poi.ss.usermodel._
import org.apache.poi.ss.util.CellReference
import org.apache.poi.ss.formula.FormulaShifter
import org.jsoup.Jsoup
import java.util.Date

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

  val Col0 = 0; val Col1 = 1; val Col2 = 2
  val Row0 = 0; val Row1 = 1

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
    wb.getSheet(XQueryFileNameCellRef.getSheetName())
      .getRow(XQueryFileNameCellRef.getRow())
      .getCell(XQueryFileNameCellRef.getCol())
      .getRichStringCellValue().getString()
  }

  /**
   * Adds user info to Workbook sheets footers.
   */
  def insertWorkBookUserDetails(wb: Workbook, userDetails: Identity): Unit = {

    // dateTimeFormat information could be obtained within userDetails or 
    // obtained thanks to it (user profile, language, locale...)
    val dateTimeFormat = "dd.MM.yyyy HH:mm"
    val dtSdf = new java.text.SimpleDateFormat(dateTimeFormat)

    for (i <- 0 until wb.getNumberOfSheets()) {
      val sheet = wb.getSheetAt(i)
      val footer = sheet.getFooter
      val preservedFooterCenterContent = if (footer.getCenter.trim.size > 0) " - " + footer.getCenter else ""
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

    for (i <- 0 until wb.getNumberOfSheets()) {
      val sheet = wb.getSheetAt(i)
      val footer = sheet.getFooter

      import org.apache.poi.xssf.usermodel.XSSFWorkbook
      import org.apache.poi.hssf.usermodel.HeaderFooter

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

          // From the query string try to find the parameter value corresponding to the parameter name specified in the spread sheet
          val parameterValue = queryString.get(parameterName) match {
            case Some(value) => value(0)
            case None =>
              Logger.error(s"No value found in query string for parameter ${parameterName}") // TODO: throw exception
              s"ERROR - No value found for parameter ${parameterName}"
          }

          Logger.debug(s"Found parameter named: ${parameterName} with value >${parameterValue}<")

          // Check the parameter value cell type and convert the matching 
          // query parameter value to the given type to preserve parameter 
          // value cell type while updating its content.
          val paramValueCell = row.getCell(1)
          paramValueCell.getCellType() match {
            case Cell.CELL_TYPE_BLANK =>
              Logger.debug(s"Updated BLANK parameter value cell with string = ${parameterValue}")
              paramValueCell.setCellValue(parameterValue)
            case Cell.CELL_TYPE_STRING =>
              Logger.debug(s"Updated STRING parameter value cell with string = ${parameterValue}")
              paramValueCell.setCellValue(parameterValue)
            case Cell.CELL_TYPE_NUMERIC =>
              Logger.debug(s"Updated parameter value cell with double = ${parameterValue}")
              Logger.warn("Request parameter used to set numeric cell. Untested operation, date and numeric conversion need extended support.")
              paramValueCell.setCellValue(parameterValue.toDouble)
            // TODO: date and numeric values need a defined format while passed as request parameter and a corresponding formatter
            //                val format = new java.text.SimpleDateFormat("dd-MM-yyyy")
            //                if (DateUtil.isCellDateFormatted(paramValueCell)) paramValueCell.   paramValueCell.setCellValue(Date.parse(parameterValue)) else paramValueCell.getNumericCellValue()
            // TODO: date and numeric values need a defined format while passed as request parameter and a corresponding formatter
            case Cell.CELL_TYPE_BOOLEAN =>
              Logger.debug(s"Updated parameter value cell with string = ${parameterValue}")
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
      wb.getSheet(ResultInsertStartCellRef.getSheetName())
        .getRow(ResultInsertStartCellRef.getRow())
        .getCell(ResultInsertStartCellRef.getCol())
        .getRichStringCellValue().getString()

    val resultDataStartCellRef = new CellReference(resultDataStartCellRefString)

    Logger.debug(s"resultDataStartCellRefString = ${resultDataStartCellRefString}, resultDataStartCellRef = ${resultDataStartCellRef}")

    // By convention the resultDataStartCellRef is considered to be on the first sheet
    val dataSheet = wb.getSheetAt(0)
    
    // TODO check whether using fake sheet may solve static formula affected by row shifting on dataSheet.
    val fakeSheet = wb.cloneSheet(0)
    
    // Get the first row as example
    val templateRow = dataSheet.getRow(resultDataStartCellRef.getRow())
    Logger.debug(s"templateRow last cell num = ${templateRow.getLastCellNum()}");
    
    import scala.collection.JavaConversions._

    // Parse report HTML table result as org.jsoup.nodes.Document
    val htmlReportDoc = Jsoup.parse(htmlTable);
    // We expect a single table per document
    val tables = htmlReportDoc.select("table")
    // Check htmlTable structure
    if (tables.size() == 0) throw HtmlTableNotFoundException(s"HTML query result is expected to contain a single table but none was found query ${getXQueryFileName(wb)}")
    else if (tables.size() > 1) throw MoreThanOneHtmlTableFoundException(s"HTML query result is expected to contain a single table but ${tables.size()} were found for query ${getXQueryFileName(wb)}")

    val dataTable = tables.get(0)
    val dataTableTrCollection = dataTable.select("tr").toIndexedSeq
    
    val maxColWithoutFormula = 4

    // ================================================================
    // ==== Deal with formula - START
    // ================================================================
    
    var rowIdxFormulaPass: Integer = resultDataStartCellRef.getRow()
    var maxCellIdxFormulaPass: Integer = 0
    
    // First results pass to create formulas an shift them 
    // Shift rows 6 - 11 on the spreadsheet to the top (rows 0 - 5)
    //sheet.shiftRows(5, 10, -5);
    
    //val tableFormulaPassIt = tables.get(0)
    
    var currFormulaPassIdx = 0
    for (row <- dataTableTrCollection) {

      Logger.debug("FormulaPass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
      Logger.debug(s"FormulaPass >>>> rowIdxFormulaPass = ${rowIdxFormulaPass} :: templateRow.getRowNum() = ${templateRow.getRowNum()}" );
      Logger.debug("FormulaPass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
      
      var cellIdxFormulaPass: Integer = resultDataStartCellRef.getCol()
      var nbColWithoutFormula = 0
      
      // Always create row at start position before shifting
      val dataRow = fakeSheet.createRow(resultDataStartCellRef.getRow())
      for (cell <- row.select("td")) {
    	  val currSheetCell = dataRow.createCell(cellIdxFormulaPass)
    	  currSheetCell.setCellValue("TEMP :: TO BE OVERRIDEN :: " + cell.text)
    	  cellIdxFormulaPass = cellIdxFormulaPass + 1
      }
      
      // keep doing while MAX_NO_FORMULA_FOUND reached
      while (nbColWithoutFormula < maxColWithoutFormula && cellIdxFormulaPass < templateRow.getLastCellNum()) {
        Logger.debug(s"FormulaPass >>>> BEFORE: nbColWithoutFormula/maxColWithoutFormula = ${nbColWithoutFormula}/${maxColWithoutFormula}")
        val exampleCell = templateRow.getCell(cellIdxFormulaPass)

        // Check if next template column contains a formula
        exampleCell.getCellType() match {
          case Cell.CELL_TYPE_FORMULA =>
            //val fRange = exampleCell.getArrayFormulaRange()            
            val cachedResult = exampleCell.getCachedFormulaResultType()
            val formula = exampleCell.getCellFormula()
            //val style = exampleCell.getCellStyle()
            Logger.debug(s"FormulaPass >>>> Formula found: >${formula}<, cachedResult: >${cachedResult}<")
            //Logger.debug(s"Formula range: \nfirst col: ${fRange.getFirstColumn()}\nlast col : ${fRange.getLastColumn()}\nfirst row: ${fRange.getFirstRow()} \nlast row : ${fRange.getLastRow()} \nnb of cells: ${fRange.getNumberOfCells()}")

            if (formula.trim().length() > 0) {
              Logger.debug(s"FormulaPass >>>> formula.trim().length() = ${formula.trim().length()}")
              val currSheetCell = dataRow.createCell(cellIdxFormulaPass)
              currSheetCell.setCellType(exampleCell.getCellType())
              currSheetCell.setCellFormula(exampleCell.getCellFormula())
              currSheetCell.setCellStyle(exampleCell.getCellStyle())

              //              FormulaShifter(0,dataSheet.getSheetName(),)
              //               int firstMovedRowIndex, int lastMovedRowIndex, int numberOfRowsToMove) 

              nbColWithoutFormula = nbColWithoutFormula
            } else {
              Logger.debug(s"FormulaPass >>>> formula.trim().length() !> 0}")
              nbColWithoutFormula = nbColWithoutFormula + 1
            }
          case _ =>
            Logger.debug(s"FormulaPass >>>> NO formula")
            nbColWithoutFormula = nbColWithoutFormula + 1
        }
        Logger.debug(s"FormulaPass >>>> nbColWithoutFormula = ${nbColWithoutFormula}")

        cellIdxFormulaPass = cellIdxFormulaPass + 1

        Logger.debug(s"FormulaPass >>>> AFTER : nbColWithoutFormula/maxColWithoutFormula = ${nbColWithoutFormula}/${maxColWithoutFormula}, cellIdxFormulaPass = ${cellIdxFormulaPass}")

      }      

      currFormulaPassIdx = currFormulaPassIdx + 1
      
      // TRY DEALING WITH FORMULA REFERENCES...
      if (currFormulaPassIdx < dataTableTrCollection.length) {
    	  // Perform shifting on fakeSheet to avoid side effect on "static" formulas
    	  fakeSheet.shiftRows(resultDataStartCellRef.getRow(), rowIdxFormulaPass, 1)
	      Logger.debug(s"FormulaPass >>>> dataSheet.shiftRows(${resultDataStartCellRef.getRow()}, ${rowIdxFormulaPass}, 1)")
      } else {
    	  Logger.debug(s"FormulaPass >>>> NO shiftRows FOR rowIdxFormulaPass = ${rowIdxFormulaPass}")
      }
      
      if (cellIdxFormulaPass > maxCellIdxFormulaPass) maxCellIdxFormulaPass = cellIdxFormulaPass
      rowIdxFormulaPass = rowIdxFormulaPass + 1 
      
    }
    
    //dataSheet.shiftRows(resultDataStartCellRef.getRow(), rowIdxFormulaPass, -1)
    //Logger.debug(s"FormulaPass BACK >>>> dataSheet.shiftRows(${resultDataStartCellRef.getRow()}, ${rowIdxFormulaPass}, 1)")
          
    // ================================================================
    // ==== Deal with formula - END
    // ================================================================    

    // ================================================================
    // ==== Deal with data - START
    // ================================================================
    
    var rowIdx: Integer = resultDataStartCellRef.getRow()
    var maxCellIdx: Integer = 0    
    
    //val tableDataPassIt = tables.get(0)
    
    for (row <- dataTableTrCollection) {

      Logger.debug("DataPass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
      Logger.debug("DataPass >>>> rowIdx = " + rowIdx);
      Logger.debug("DataPass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");      
      
      var cellIdx: Integer = resultDataStartCellRef.getCol()
      var nbColWithoutFormula = 0
      // While using fakeSheet we now need creating rows again
      val dataRow = dataSheet.createRow(rowIdx)
      //val dataRow = dataSheet.getRow(rowIdx)
      
      for (cell <- row.select("td")) {
       
        //val currSheetCell = dataRow.createCell(cellIdx)
        val currSheetCell =  if (dataRow.getCell(cellIdx) != null) {
          dataRow.getCell(cellIdx)
        } else {
          dataRow.createCell(cellIdx)
        }
        
        if (!cell.text.isEmpty()) {
          // Cell type is defined after td class names.
          // Currently supported names for type specific {"date","num"} 
          cell.className() match {
            case DateCssClassName => 
              currSheetCell.setCellValue(sdf.parse(cell.text))
            case NumericCssClassName => 
              currSheetCell.setCellValue(java.lang.Double.parseDouble(cell.text))
            case _ => 
              currSheetCell.setCellValue(cell.text)
          }
        } else {
          currSheetCell.setCellValue(cell.text)
        }
        
        // Preserve example row cells style
        val cellStyle = templateRow.getCell(cellIdx).getCellStyle()
       
        currSheetCell.setCellStyle(cellStyle)
        cellIdx = cellIdx + 1
        
      }
      // TODO: All data columns processed, proceed with formulas columns if any...

      // keep doing while MAX_NO_FORMULA_FOUND reached
      while (nbColWithoutFormula < maxColWithoutFormula && cellIdx < templateRow.getLastCellNum()) {
        Logger.debug(s"BEFORE: nbColWithoutFormula/maxColWithoutFormula = ${nbColWithoutFormula}/${maxColWithoutFormula}")
        val exampleCell = templateRow.getCell(cellIdx)

        // Check if next template column contains a formula
        exampleCell.getCellType() match {
          case Cell.CELL_TYPE_FORMULA =>
            //val fRange = exampleCell.getArrayFormulaRange()            
            val cachedResult = exampleCell.getCachedFormulaResultType()
            val formula = exampleCell.getCellFormula()
            //val style = exampleCell.getCellStyle()
            Logger.debug(s"Formula found: >${formula}<, cachedResult: >${cachedResult}<")
            //Logger.debug(s"Formula range: \nfirst col: ${fRange.getFirstColumn()}\nlast col : ${fRange.getLastColumn()}\nfirst row: ${fRange.getFirstRow()} \nlast row : ${fRange.getLastRow()} \nnb of cells: ${fRange.getNumberOfCells()}")

            if (formula.trim().length() > 0) {
              Logger.debug(s"formula.trim().length() = ${formula.trim().length()}")
              val currSheetCell = dataRow.createCell(cellIdx)
              currSheetCell.setCellType(exampleCell.getCellType())
              //currSheetCell.setCellFormula(exampleCell.getCellFormula())
              val shiftedFormulaFromFakeSheet = fakeSheet.getRow(currSheetCell.getRowIndex()).getCell(currSheetCell.getColumnIndex()).getCellFormula()
              Logger.debug(s"shiftedFormulaFromFakeSheet = ${shiftedFormulaFromFakeSheet}")
              currSheetCell.setCellFormula(shiftedFormulaFromFakeSheet)
              currSheetCell.setCellStyle(exampleCell.getCellStyle())

              //              FormulaShifter(0,dataSheet.getSheetName(),)
              //               int firstMovedRowIndex, int lastMovedRowIndex, int numberOfRowsToMove) 

              nbColWithoutFormula = nbColWithoutFormula
            } else {
              Logger.debug(s"formula.trim().length() !> 0}")
              nbColWithoutFormula = nbColWithoutFormula + 1
            }
          case _ =>
            Logger.debug(s"NO formula")
            nbColWithoutFormula = nbColWithoutFormula + 1
        }
        Logger.debug(s"nbColWithoutFormula = ${nbColWithoutFormula}")

        cellIdx = cellIdx + 1

        Logger.debug(s"AFTER : nbColWithoutFormula/maxColWithoutFormula = ${nbColWithoutFormula}/${maxColWithoutFormula}, cellIdx = ${cellIdx}")

      }

      // If no  => copy template content until MAX_NO_FORMULA_FOUND
      // If yes => copy template formula and adapt it to current row index

      //dataRow.createCell(cellIdx)

      if (cellIdx > maxCellIdx) maxCellIdx = cellIdx
      rowIdx = rowIdx + 1
    }
    
    // Get rid of the temporary fake sheet.
    wb.removeSheetAt(wb.getSheetIndex(fakeSheet))
    
    // ================================================================
    // ==== Deal with data - END
    // ================================================================

    
    // ================================================================
    // ==== Deal with Print - START
    // ================================================================
    
    val maxColIdx = maxCellIdx
    val maxRowIdx = rowIdx

    definePrintRange(wb, resultDataStartCellRef, maxColIdx, maxRowIdx)
    Logger.debug(s"maxColIdx = ${maxColIdx}, maxRowIdx = ${maxRowIdx}")

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

    val printArea = wb.getPrintArea(0)
    val printRange = printArea.split("!")(1)
    Logger.debug(s"printArea: ${printArea}, printRange: ${printRange}")

    val printRangeStart = printRange.split(":")(0)
    val printRangeStartCellRef = new CellReference(printRangeStart)

    // Compute resultDataEndCellAbsRef column position
    val endCol = if (printRangeStartCellRef.getCol() > resultDataStartCellRef.getCol()) {
      maxColIdx - (printRangeStartCellRef.getCol() - resultDataStartCellRef.getCol())
    } else {
      maxColIdx + (resultDataStartCellRef.getCol() - printRangeStartCellRef.getCol())
    }
    // Define new print range end position
    val resultDataEndCellAbsRef = new CellReference(maxRowIdx - 1, endCol, AbsRow, AbsCol)
    val newPrintRange = printRangeStart + ":" + resultDataEndCellAbsRef.formatAsString()
    wb.setPrintArea(0, newPrintRange)
  }

  /**
   * HSSF and XSSF compatible formulas evaluation.
   */
  def evaluateAllFormulaCells(wb: Workbook): Unit = {
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    for (i <- 0 until wb.getNumberOfSheets()) {
      val sheet = wb.getSheetAt(i);
      for (row <- sheet) {
        for (cell <- row) {
          if (cell.getCellType() == Cell.CELL_TYPE_FORMULA) {
            evaluator.evaluateFormulaCell(cell);
          }
        }
      }
    }
  }

}

/**
 *  Exception thrown when the expected HTML table is not found within the HTML query result.
 */
case class HtmlTableNotFoundException(message: String = null, cause: Throwable = null) extends Exception(message, cause)
/**
 *  Exception thrown when the HTML result contains more than a single expected HTML table.
 */
case class MoreThanOneHtmlTableFoundException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

