package test.ch.bsisa.hyperbird.mail

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

import play.api.GlobalSettings

import ch.bsisa.hyperbird.mail._

/**
 * Test SendMailSpec
 *
 *  Tip: from sbt play console run:
 * {{{
 * test-only test.ch.bsisa.hyperbird.mail.SendMailSpec
 * }}}
 * to have only the current test run.
 *
 * @author Patrick Refondini
 */
class SendMailSpec extends PlaySpecification {

  "The specification" should {

    "have access to HeaderNames" in {
      USER_AGENT must be_===("User-Agent")
    }

    "have access to Status" in {
      OK must be_===(200)
    }
  }

  "Test mail" should {
    s"send " in new WithApplication {
      val res = Sender.send(
        new Mail(
          from = ("refon@pobox.com", "Refon"),
          to = Seq("patrick.refondini@escalesoft.com"),
          cc = Seq("patrick@refondini.com"),
          bcc = Seq.empty,
          subject = "HB5 Mailing test 1",
          message = s"""
Bonjour,
          
ceci est un test d'envoi d'email texte simple depuis HB5.
Meilleures salutations,
	
HyperBird 5
""",
          richMessage = Some(s"""
Bonjour,
          
ceci est un test d'envoi d'email <em>en HTML</em> depuis HB5.
              
Meilleures salutations,
	
<i>HyperBird 5</i>

"""),
          //attachment = None))
          attachment = Some(new java.io.File("./README"))))

      val searchStr: String = ".JavaMail."
      val startIdx: Int = res.indexOf(searchStr)
      val endIdx: Int = startIdx + searchStr.size
      val resSubStr: String = res.substring(startIdx, endIdx)
      resSubStr mustEqual searchStr
    }
  }
  

}