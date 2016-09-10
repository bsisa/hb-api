package ch.bsisa.hyperbird.dao

import ch.bsisa.hyperbird.{ ApiConfig, CollectionsConfig }
import ch.bsisa.hyperbird.dao.ws.WSQueries
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.model.{ ELFIN, FORME, POINT }
import ch.bsisa.hyperbird.util.ElfinUtil

//import ch.bsisa.hb.geo.json.HbGeoFormatImplicits._
import ch.bsisa.hb.geo.model.json.HbGeoFormatImplicits._

import play.api.libs.concurrent.Execution.Implicits._ // Required by FUTURE

import play.api.libs.ws.Response
import play.api.libs.ws.WS
import play.api.Logger
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future



// TODO: import geo util...
import play.api.libs.json.{ Reads, Writes, Format, JsPath }
import play.api.libs.json.{ Json, JsArray, JsError, JsNumber, JsResult, JsSuccess, JsValue }
import play.api.libs.functional.syntax._

import ch.bsisa.hb.geo.model._

/**
 * Dedicated to ELFIN.FORME.POINT coordinates conversions
 *
 * Could be extended to manage ELFIN.CENTROIDE as needed.
 */
object GeoXmlDAO {

  private def buildSwissToGpsPostBodyRequest(elfin: ELFIN): JsValue = {

    val toUpdatePointOptions = for (point <- elfin.FORME.get.POINT.seq) yield {

      val computeGps = for {
        x <- point.X
        y <- point.Y
      } yield {
        s"""{ "id" : "${point.POS}", "coord" : {"xEastingLng":${x},"yNorthingLat":${y},"zAltitude":${point.Z}} }"""
      }
      computeGps
    }

    // Build the array of points as a JSON string to convert while skipping None entries
    val postStringArrayContent = toUpdatePointOptions.foldLeft("") { (acc, pointOpt) =>
      pointOpt match {
        case Some(point) => point + "," + acc
        case None        => acc
      }
    }

    // Enclose the JSON array content within brackets removing trailing comma. 
    val postBodyString = "[" + postStringArrayContent.substring(0, postStringArrayContent.size - 1) + "]"
    val postBody = play.api.libs.json.Json.parse(postBodyString)
    postBody
  }

  /**
   * Returns a new ELFIN whose POINTs have been replaced with new POINTS containing GPS coordinates held in `response`, every other ELFIN information being preserved.
   *
   * Note: Using find and sortWith on POINT sequence prevents any assumption on sequence ordering.
   * If performance problem would be noticed, granted ordering assumption is guaranteed replacing
   * find by for with zip and POINT(i) access together with simple .reverse at the end of
   * updatedPoints construction could do.
   */
  private def updateElfinPoints(elfin: ELFIN, response: Seq[CoordinatesWithId]): ELFIN = {

    val updatedPoints = for (coordsWithId <- response) yield {
      val point = elfin.FORME.get.POINT.find { p => p.POS.toString == coordsWithId.id }
      ElfinUtil.updatePointGpsCoordinates(point.get, Some(coordsWithId.coordinates.xEastingLng), Some(coordsWithId.coordinates.yNorthingLat), Some(coordsWithId.coordinates.zAltitude))
    }
    val updatedPointsSortByPos = updatedPoints.sortWith((a, b) => a.POS < b.POS)

    ElfinUtil.updateElfinPoints(elfin, updatedPointsSortByPos)
  }

  /**
   * Batch conversion of all POINT X,Y,Z swiss federal coordinates (LV03) to POINT XG,YG,ZG GPS coordinates.
   */
  def convertAllPointsToGps()(implicit dbConfig: DbConfig, apiConfig: ApiConfig, collectionsConf: CollectionsConfig): Unit = {

    val hbGeoProtocol = apiConfig.hbGeoApiProtocol
    val hbGeoHost = apiConfig.hbGeoApiHost
    val hbGeoPort = apiConfig.hbGeoApiPort

    // HbGeoController.getGpsForSwissCoordinatesList()
    val getGpsForSwissCoordinatesListUrlPart = "coordinates/gps/"
    val path = dbConfig.databaseName + "/"
    val xpath = """[FORME/POINT]"""
    val orderBy = """@Id"""
    val pageSize = 10

    // elfins_ordered_by.xq takes parameters: startIndex, maxNbResults, path, xpath, orderBy 
    val xqueryFileName = "elfins_ordered_by.xq"

    val countQueryString = s"""&isCount=true&startIndex=1&maxNbResults=1000000000&path=${path}&xpath=${xpath}&orderBy=${orderBy}"""

    XQueryWSHelper.runXQueryFile(xqueryFileName, Some(countQueryString)).map { nbOfElfinsResp =>

      import scala.util.{ Failure, Success, Try }

      def parseCount(response: Response): Try[Int] = Try(response.xml.text.toInt)

      val nbOfElfinsOpt = parseCount(nbOfElfinsResp) match {
        case s: Success[Int] => Some(s.value)
        case e: Failure[Int] =>
          Logger.error("convertAllPointsToGps could not obtain nbOfElfins count: " + e.toString())
          None
      }

      nbOfElfinsOpt.map { nbOfElfins =>

        Logger.debug(s"About to update ${nbOfElfins} elfins with POINT.")

        val range = (1 to nbOfElfins by pageSize).view // Better have lazy range

        for (from <- range) {

          val maxResults = if ((from + pageSize - 1) > nbOfElfins) (nbOfElfins % pageSize) else pageSize
          Logger.debug(s"Paging startIndex ${from} for maxResults ${maxResults}")

          val queryString = s"""&startIndex=${from}&maxNbResults=${maxResults}&path=${path}&xpath=${xpath}&orderBy=${orderBy}"""

          // Sequential blocking calls - slow throughput but may avoid database overflow when no back pressure is available
          val futureElfins = XQueryWSHelper.queryElfins(WSQueries.runXQueryFile(xqueryFileName, Some(queryString)))
          val elfins = Await.result(futureElfins, 1 minutes)

          // Parallel non blocking high throughput - may overflow database when no back pressure is available
          //XQueryWSHelper.queryElfins(WSQueries.runXQueryFile(xqueryFileName, Some(queryString))).map { elfins =>

            //Logger.debug(s"Obtained ${elfins.size} elfins")

            // For each elfin containing point
            elfins.foreach { elfin =>

              //Logger.debug(s">>>> Proceed with elfin.Id ${elfin.Id}")

              val postBody = buildSwissToGpsPostBodyRequest(elfin)
              val postUrl = s"${hbGeoProtocol.get}://${hbGeoHost.get}:${hbGeoPort.get}/${getGpsForSwissCoordinatesListUrlPart}"

              //Logger.debug(s">>>> POST URL ${postUrl}")

              import scala.concurrent.duration._

              // Set a very long timeout in case no cache is available on coordinates service.
              val wsRespFuture: Future[Response] = WS.url(postUrl).withRequestTimeout(3600000).post(postBody)
              
              // Sequential blocking calls - slow throughput but may avoid database overflow when no back pressure is available
              val wsResp = Await.result(wsRespFuture, 1 minutes)
              
              // Parallel non blocking high throughput - may overflow database when no back pressure is available              
              //wsRespFuture map { wsResp =>
                // Check web service response status 
                wsResp.status match {
                  // Ok proceed
                  case 200 => {
                    val coordinatesWithIdSeq = wsResp.json match {
                      case swissCoords: JsArray => {
                        for (swissCoord <- swissCoords.value) yield { swissCoord.as[CoordinatesWithId] }
                      }
                      case jsValue =>
                        Logger.error(s"Expecting a JsArray of coordinates not: ${jsValue}")
                        val emptyRes = Seq[CoordinatesWithId]()
                        emptyRes
                    }
                    val updatedElfin = updateElfinPoints(elfin, coordinatesWithIdSeq)

                    // Persist updated elfin
                    ElfinDAO.update(updatedElfin)
                  }
                  // Not Ok log exception
                  case _ => {
                    Logger.error(s"Problem calling hb-geo-api service: ${wsResp.status} - body is: ${wsResp.body}")
                  }
                }
              //} // Parallel non blocking call end 

              wsRespFuture recover { case e => Logger.error(s"Problem calling hb-geo-api service: ${e.toString}") }
            }
          //} // Parallel non blocking call end
        } // for loop end
      }
    }
  }

}