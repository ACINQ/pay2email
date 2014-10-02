package fr.acinq.pay2email

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.regions.{Regions, Region}
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClient
import com.amazonaws.services.simpleemail.model._

object EmailSender {

  val sesClient = new AmazonSimpleEmailServiceClient()
  sesClient.setRegion(Region.getRegion(Regions.EU_WEST_1))

  def send(from: String, to: String, subject: String, htmlBody: String) = {

    // Construct an object to contain the recipient address.
    val destination = new Destination().withToAddresses(to)

    // Create a message with the specified subject and body.
    val message = new Message()
      .withSubject(new Content().withData(subject))
      .withBody(new Body().withHtml(new Content().withData(htmlBody)))

    // Assemble the email.
    val request = new SendEmailRequest().withSource(from).withDestination(destination).withMessage(message)

    // Send the email.
    sesClient.sendEmail(request)

  }

}
