package ch.bsisa.hyperbird.patman.simulations.actors
import akka.actor.{ Actor, ActorLogging }
import ch.bsisa.hyperbird.model.ELFIN
import ch.bsisa.hyperbird.util.DateUtil
import ch.bsisa.hyperbird.model.format.Implicits._
import ch.bsisa.hyperbird.patman.simulations.Constants._
import ch.bsisa.hyperbird.model.format.ElfinFormat
import ch.bsisa.hyperbird.patman.simulations.messages._
import ch.bsisa.hyperbird.patman.simulations.model.Bed
import ch.bsisa.hyperbird.util.ElfinUtil
import java.util.Date
import play.api.libs.ws.WS
import play.api.libs.ws.Response
import scala.concurrent.Future
import com.ning.http.client.Realm.AuthScheme

/**
 * RefreshSimulationActor is responsible for cleaning up existing simulation data
 * and recreating up to date simulation data.
 * 
 * @author Patrick Refondini
 */
class RefreshSimulationActor extends Actor with ActorLogging {

  def receive = {
    case RefreshSimulationMessage(dbUser, dbPassword, resourcesToDelete) =>
      // TODO: implement clean up / run routine
      // 1) Check from/to date range to run the simulation for. Cancel refresh if facing a problem (preserve existing data)   
      // 2) Clean up existing simulation data
      for ( resourceToDelUrl <- resourcesToDelete) {
            log.debug("resourceToDelUrl for DELETE : " + resourceToDelUrl)
            val responseFuture: Future[Response] = WS.url(resourceToDelUrl).withAuth(dbUser, dbPassword, AuthScheme.BASIC).delete
      }
      // 3) Run simulation for given range
      // 4) Notify system configured users of the latest simulation update/failure by email.
      log.info(s"clean... at ${new Date()}")
    case "simulate" => log.warning("simulate...")
  }

}



