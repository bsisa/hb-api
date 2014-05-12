package ch.bsisa.hyperbird.security

import securesocial.core._
import play.api.mvc.{Session, RequestHeader}
import play.api.{Application, Logger}

/**
 * SecureSocial service event listener.
 * 
 * @author Patrick Refondini
 */
class SecureServiceEventListener(app: Application) extends EventListener {
  override def id: String = "my_event_listener"
   
  def onEvent(event: Event, request: RequestHeader, session: Session): Option[Session] = {
    val eventName = event match {
      case e: LoginEvent => "login"
      case e: LogoutEvent => "logout"
      case e: SignUpEvent => "signup"
      case e: PasswordResetEvent => "password reset"
      case e: PasswordChangeEvent => "password change"
    }

    Logger.info("traced %s event for user %s".format(eventName, event.user.fullName))
    Logger.debug(s"request.path = ${request.path} , request.rawQueryString = ${request.rawQueryString}")
    // Not changing the session so just return None
    // if you wanted to change the session then you'd do something like
    // Some(session + ("your_key" -> "your_value"))
    None
  }
}