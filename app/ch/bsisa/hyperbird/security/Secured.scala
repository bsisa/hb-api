package ch.bsisa.hyperbird.security

import play.api.mvc._

/**
 * Provide security features.
 *
 * Security trait design after play.api.mvc.Security.Authenticated examples.
 */
trait Secured { self: Controller =>

  val UserNameSessionToken = "user"

  /**
   * Retrieve the connected user id.
   */
  def username(request: RequestHeader) = request.session.get(UserNameSessionToken)

  /**
   * Redirect to login if the use in not authorized.
   */
  def onUnauthorized(request: RequestHeader): SimpleResult

  /**
   * Wraps another action, allowing only authenticated HTTP requests.
   * Retrieves the user info from session using UserNameSessionToken constant
   * and expects objects using this traits to implement onUnauthorized function
   * to specify what to do in case unsuccessful authentication.
   */
  def IsAuthenticated(f: => String => Request[AnyContent] => Result) =
    Security.Authenticated(username, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
}