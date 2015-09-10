package ch.bsisa.hyperbird

import play.api.Play

/**
 *  Basic mail support implementation relying on Apache common mail
 *  
 *  Closely inspired from: 
 *  
 *  http://stackoverflow.com/questions/10492858/sending-emails-in-playframework-2-0
 *  https://gist.github.com/mariussoutier/3436111
 *  
 *  Note: Quick fix to wrong format usage leading to email with attachment never 
 *  being displayed in HTML. HtmlEmail inherits from MultiPartEmail so the original
 *  format logic is broken.
 */
package object mail {

  sealed abstract class MailType

  case object Plain extends MailType
  case object Rich extends MailType
  case object MultiPart extends MailType

  case class Mail(
    from: (String, String), // (email -> name)
    to: Seq[String],
    cc: Seq[String] = Seq.empty,
    bcc: Seq[String] = Seq.empty,
    subject: String,
    message: String,
    richMessage: Option[String] = None,
    attachment: Option[(java.io.File)] = None)

  val smtpHostName = Play.current.configuration.getString("smtp.host").get
  val smtpPort = Play.current.configuration.getString("smtp.port").get
  val smtpUser = Play.current.configuration.getString("smtp.user").get
  val smtpPassword = Play.current.configuration.getString("smtp.password").get

  object Sender {
    def send(mail: Mail): String = {
      import org.apache.commons.mail._

      val format =
        if (mail.attachment.isDefined) MultiPart
        else if (mail.richMessage.isDefined) Rich
        else Plain

      val commonsMail: Email = format match {
        case Plain => new SimpleEmail().setMsg(mail.message)
        case Rich => new HtmlEmail().setHtmlMsg(mail.richMessage.get).setTextMsg(mail.message)
        case MultiPart => {
          val attachment = new EmailAttachment()
          attachment.setPath(mail.attachment.get.getAbsolutePath)
          attachment.setDisposition(EmailAttachment.ATTACHMENT)
          attachment.setName(mail.attachment.get.getName)
          if (mail.richMessage.isDefined) {
            new HtmlEmail().setHtmlMsg(mail.richMessage.get).setTextMsg(mail.message).attach(attachment)
          } else {
        	new MultiPartEmail().attach(attachment).setMsg(mail.message)  
          }
        }
      }

      commonsMail.setHostName(smtpHostName)
      commonsMail.setSmtpPort(smtpPort.toInt)
      commonsMail.setAuthentication(smtpUser, smtpPassword)
      commonsMail.setStartTLSEnabled(true)
      //commonsMail.setTLS(true)

      mail.to foreach (commonsMail.addTo(_))
      mail.cc foreach (commonsMail.addCc(_))
      mail.bcc foreach (commonsMail.addBcc(_))

      val res: String = commonsMail.
        setFrom(mail.from._1, mail.from._2).
        setSubject(mail.subject).
        send()
      res
    }
  }
}
