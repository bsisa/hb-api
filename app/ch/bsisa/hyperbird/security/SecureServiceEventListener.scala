package ch.bsisa.hyperbird.security

import securesocial.core.{Event,EventListener,LoginEvent,LogoutEvent,PasswordChangeEvent,PasswordResetEvent,SignUpEvent}
import play.api.mvc.{Session, RequestHeader}
import play.api.{Application, Logger}
import play.api.cache.Cache
import play.api.Play.current
import java.util.Date
import ch.bsisa.hyperbird.util.DateUtil

/**
 * SecureSocial service event listener.
 * 
 * This listener can be enabled, disabled in conf/play.plugins as: 
 * `10100:ch.bsisa.hyperbird.security.SecureServiceEventListener`
 * Where 10100 is a priority number that determines the order in which plugins start up.
 * 
 * @author Patrick Refondini
 */
class SecureServiceEventListener(app: Application) extends EventListener {
  
  override def id: String = "hbSecureServiceEventListenerId"
  
  private val logger = Logger("ch.bsisa.hyperbird.security.SecureServiceEventListener")
    
  def onEvent(event: Event, request: RequestHeader, session: Session): Option[Session] = {
    val eventName = event match {
      case e: LoginEvent => {
        val currentDate = new Date();
        val user = event.user match {case user: User => user}
        if (!(user.validFromDate.before(currentDate) && user.validToDate.after(currentDate))) { 
          throw new Exception(s"User ${user.identityId.userId} - ${user.fullName} access is currently disactivated (${DateUtil.hbDateFormat.format(new Date)}), please contact your system administrator.") 
        }
      }
      case e: LogoutEvent => {
        // Clean up cache on logout. Useful for user to notice their roles change before cache TTL has expired.
        Cache.remove(event.user.identityId.userId)
      }
      case e: SignUpEvent => "signup"
      case e: PasswordResetEvent => {
        "password reset"
//      Checkout securesocial.core.providers.utils.Mailer.sendEmail for example Akka scheduler usage with MailerPlugin
//	    Akka.system.scheduler.scheduleOnce(1 seconds) {
//	      val mail = use[MailerPlugin].email
//	      mail.setSubject(subject)
//	      mail.setRecipient(recipient)
//	      mail.setFrom(fromAddress)
//	      // the mailer plugin handles null / empty string gracefully
//	      mail.send(body._1.map(_.body).getOrElse(""), body._2.map(_.body).getOrElse(""))
//	    } 
      }
      case e: PasswordChangeEvent => "password change"
    }

    logger.info(s"Traced ${eventName} event for user ${event.user.fullName}. Request.path = ${request.path} , Request.rawQueryString = ${request.rawQueryString}")
    // Not to change to the session simply return: None
    // Otherwise to change the session, return something like: Some(session + ("your_key" -> "your_value"))
    None
  }
}