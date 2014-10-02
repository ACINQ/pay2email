package fr.acinq.pay2email

import com.ning.http.client._
import scala.concurrent.{Promise, Future}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

object CaptchaVerifier {

  val config = new AsyncHttpClientConfig.Builder().build
  val client: AsyncHttpClient = new AsyncHttpClient(config)

  def verify(private_key: String, ip: String, challenge: String, response: String): Future[Boolean] = {
    val promise = Promise[Boolean]()
    client
      .preparePost("https://www.google.com/recaptcha/api/verify")
      .addParameter("privatekey", private_key)
      .addParameter("remoteip", ip)
      .addParameter("challenge", challenge)
      .addParameter("response", response)
      .execute(new AsyncCompletionHandler[Unit] {

      override def onCompleted(response: Response): Unit = promise.success(response.getResponseBody.split("\n")(0).toBoolean)

      override def onThrowable(t: Throwable) = promise.failure(t)
    })
    promise.future
  }

}