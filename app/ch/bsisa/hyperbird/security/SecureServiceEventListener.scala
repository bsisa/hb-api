package ch.bsisa.hyperbird.security

import securesocial.core.{Event,EventListener,LoginEvent,LogoutEvent,PasswordChangeEvent,PasswordResetEvent,SignUpEvent}
import play.api.mvc.{Session, RequestHeader}
import play.api.{Application, Logger}

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
   
  def onEvent(event: Event, request: RequestHeader, session: Session): Option[Session] = {
    val eventName = event match {
      case e: LoginEvent => {
        "login"
        // Checkout securesocial.core.providers.utils.Mailer.sendEmail for example Akka scheduler usage with MailerPlugin
//	    Akka.system.scheduler.scheduleOnce(1 seconds) {
//	      val mail = use[MailerPlugin].email
//	      mail.setSubject(subject)
//	      mail.setRecipient(recipient)
//	      mail.setFrom(fromAddress)
//	      // the mailer plugin handles null / empty string gracefully
//	      mail.send(body._1.map(_.body).getOrElse(""), body._2.map(_.body).getOrElse(""))
//	    } 
      }
      case e: LogoutEvent => "logout"
      case e: SignUpEvent => "signup"
      case e: PasswordResetEvent => "password reset"
      case e: PasswordChangeEvent => "password change"
    }

    Logger.info(">>>> SECURITY: traced %s event for user %s".format(eventName, event.user.fullName))
    Logger.debug(s">>>> SECURITY: request.path = ${request.path} , request.rawQueryString = ${request.rawQueryString}")
    // Not changing the session so just return None
    // if you wanted to change the session then you'd do something like
    // Some(session + ("your_key" -> "your_value"))
    None
  }
}