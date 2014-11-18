package ch.bsisa.hyperbird.security

import securesocial.core.{AuthenticationMethod, Identity, IdentityId, SocialUser, OAuth1Info, OAuth2Info, PasswordInfo}
import java.util.Date

/**
 * securesocial.core.Identity custom implementation extending securesocial.core.SocialUser 
 * adding our additional roles, valid from and to date informations.
 */
class User(
    identityId: IdentityId, 
    firstName: String, 
    lastName: String, 
    fullName: String , 
    email: Option[String], 
    avatarUrl: Option[String] = None, 
    authMethod: AuthenticationMethod = AuthenticationMethod.UserPassword, 
    oAuth1Info: Option[OAuth1Info] = None, 
    oAuth2Info: Option[OAuth2Info] = None, 
    passwordInfo: Option[PasswordInfo], 
    validFrom: Date,
    validTo: Date,
    rolesParam: Option[Seq[Role]] = None) extends SocialUser(identityId, firstName, lastName, fullName, email, avatarUrl, authMethod, oAuth1Info, oAuth2Info, passwordInfo) {
   
  def roles: Option[Seq[Role]] = rolesParam
  def validFromDate: Date = validFrom
  def validToDate: Date = validTo

}