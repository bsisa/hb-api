package ch.bsisa.hyperbird.patman.simulations.actors

import akka.actor.{ Actor, ActorLogging }

import ch.bsisa.hyperbird.Implicits._
import ch.bsisa.hyperbird.dao.ws.XQueryWSHelper
import ch.bsisa.hyperbird.util.DateUtil
import ch.bsisa.hyperbird.mail._
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
                    // 4) Notify system configured users of the latest simulation update/failure by email.
                    sendNotification(s"FluxPatients Simulation Refresh performed no the ${new Date()}",
                      s"""
Automatick FluxPatients Simulation successfully launched
                        
Id:        ${simId}
Author:    ${author}
Date from: ${simulationDateFrom} 
Date to:   ${simulationDateTo}

HyperBird - Patman
""")
                  }

                case None =>
                  log.warning("Simulation refresh cancelled. Date range 'date to' field could not be obtained.")
                  // TODO: notify
                  sendNotification(s"FluxPatients Simulation Refresh CANCELLED on the ${new Date()}",
                    s"""
Automatick FluxPatients Simulation failed to launched

Cause: Date range 'date to' field could not be obtained.

HyperBird - Patman
""")
              }
            case None =>
              log.warning("Simulation refresh cancelled. Date range 'date from' field could not be obtained.")
              // TODO: notify
              sendNotification(s"FluxPatients Simulation Refresh CANCELLED on the ${new Date()}",
                s"""
Automatick FluxPatients Simulation failed to launched

Cause: Date range 'date from' field could not be obtained.

HyperBird - Patman
""")
          }

        }

      } catch {
        case e: Throwable =>
          log.error(s"Simulation refresh failed. Error: ${e}")
          // TODO: notify
          sendNotification(s"FluxPatients Simulation Refresh FAILED on the ${new Date()}",
            s"""
Automatick FluxPatients Simulation failed to launched

Cause: ${e.getMessage()}
              
StackTrace: 
              
${e.getStackTrace()}

HyperBird - Patman
""")
      }

    case _ =>
      log.warning("Unsupported RefreshSimulationActor message received. Droping it.")
      sendNotification(s"FluxPatients Unsupported RefreshSimulationActor message received on the ${new Date()}",
        s"""

Unsupported RefreshSimulationActor message received

This should have no impact on the software behaviour. 
Although this warning shows possible misconfiguration 
incorrect behaviour introduced in source code change. 

Please request your IT staff to check this out.

HyperBird - Patman
""")
  }

  /**
   * TODO: Parametrise From, To addresses. Could come from configuration file or database, each
   * user having mandatory email.
   */
  def sendNotification(subject: String, message: String): Unit = {
    val res = Sender.send(
      new Mail(
        from = ("patrick.refondini@escalesoft.com", "Patrick Refondini"),
        to = Seq("Patrick Refondini <patrick.refondini@escalesoft.com>"),
        cc = Seq.empty,
        bcc = Seq.empty,
        subject = subject,
        message = message,
        richMessage = None,
        attachment = None))
  }

}



