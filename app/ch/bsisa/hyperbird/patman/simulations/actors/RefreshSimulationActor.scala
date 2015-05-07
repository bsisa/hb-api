package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorLogging }

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.util.DateUtil
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.patman.simulations.SimulationLauncher

import com.ning.http.client.Realm.AuthScheme

import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.api.libs.ws.Response

import scala.concurrent.Future
import java.util.Date

/**
 * RefreshSimulationActor is responsible for:
 *
 * 1) Check from/to date range to run the simulation for
 * 2) Clean up existing simulation data
 * 3) Create up to date simulation data
 * 4) TODO: Notify system configured users of the latest simulation update/failure by email.
 *
 * @author Patrick Refondini
 */
class RefreshSimulationActor extends Actor with ActorLogging {

  def receive = {
    case RefreshSimulationMessage(dbUser, dbPassword, resourcesToDelete) =>

      log.info(s"RefreshSimulationMessage received at ${new Date()}")
      try {
        // Clean up / run routine
        // 1) Check from/to date range to run the simulation for. Cancel refresh if facing a problem (preserve existing data)
        val rawQueryString = None
        val fromToFuture = XQueryWSHelper.runXQueryFile(FIND_DATE_RANGE_FOR_SIMULATION_XQUERY, rawQueryString).map { response =>

          val respStr = response.body.mkString
          log.debug(s"${FIND_DATE_RANGE_FOR_SIMULATION_XQUERY} reponse = ${respStr}")

          val dateRangeJs: JsValue = Json.parse(respStr)

          val dateFromOpt = (dateRangeJs \ FIND_DATE_RANGE_FOR_SIMULATION_XQUERY_DATE_FROM_FIELD).asOpt[String]
          val dateToOpt = (dateRangeJs \ FIND_DATE_RANGE_FOR_SIMULATION_XQUERY_DATE_TO_FIELD).asOpt[String]

          val dateFromDateOpt = dateFromOpt.map { case dateFrom => DateUtil.getIsoDateFormatterWithoutTz.parse(dateFrom) }
          val dateToDateOpt = dateToOpt.map { case dateTo => DateUtil.getIsoDateFormatterWithoutTz.parse(dateTo) }

          dateFromDateOpt match {
            case Some(dateFromDate) =>
              dateToDateOpt match {
                case Some(dateToDate) =>

                  val simulationDateFrom = DateUtil.hbDateFormat.format(dateFromDate)
                  val simulationDateTo = DateUtil.hbDateFormat.format(dateToDate)

                  log.info(s"${FIND_DATE_RANGE_FOR_SIMULATION_XQUERY_DATE_FROM_FIELD} = ${simulationDateFrom}, ${FIND_DATE_RANGE_FOR_SIMULATION_XQUERY_DATE_TO_FIELD} = ${simulationDateTo}")

                  // 2) Clean up existing simulation data
                  for (resourceToDelUrl <- resourcesToDelete) {
                    log.info("resourceToDelUrl for DELETE : " + resourceToDelUrl)
                    val responseFuture: Future[Response] = WS.url(resourceToDelUrl).withAuth(dbUser, dbPassword, AuthScheme.BASIC).delete
                    responseFuture.map { resp =>
                      log.info("resourceToDelUrl reponse for for DELETE : " + resp.body.mkString)
                    }
                  }

                  // 3) Run simulation for given range. 
                  // Expected result format is JSON as: { "dateFrom" : "2015-01-12T08:00:00+01:00", "dateTo" : "2015-04-28T22:00:00+02:00" }
                  val author = BACKGROUND_JOBS_AUTHOR
                  val futureSimulationId = SimulationLauncher.launchSimulation(author, simulationDateFrom, simulationDateTo)
                  futureSimulationId.map { simId =>
                    log.info(s"Simulation launched with Id ${simId} for author ${author}, dateFrom ${simulationDateFrom}, dateTo ${simulationDateTo}")
                  }

                  // 4) Notify system configured users of the latest simulation update/failure by email.
                  // TODO: notify

                case None =>
                  log.warning("Simulation refresh cancelled. Date range 'date to' field could not be obtained.")
                // TODO: notify
              }
            case None =>
              log.warning("Simulation refresh cancelled. Date range 'date from' field could not be obtained.")
            // TODO: notify
          }

        }

      } catch {
        case e: Throwable =>
          log.error(s"Simulation refresh failed. Error: ${e}")
        // TODO: notify
      }

    case _ => log.warning("Unsupported RefreshSimulationActor message received. Droping it.")
  }

}



