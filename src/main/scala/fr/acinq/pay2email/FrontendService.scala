package fr.acinq.pay2email

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import fr.acinq.bitcoin._
import akka.actor.Actor
import grizzled.slf4j.Logging
import spray.http._
import spray.routing._
import spray.routing.directives.LogEntry
import spray.util.LoggingContext
import scala.collection.JavaConversions._

import scala.concurrent.ExecutionContext
import scala.util.{Success, Try}

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
abstract class FrontendServiceActor extends Actor with FrontendService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing,
  // timeout handling or alternative handler registration
  def receive = runRoute(route)
}


trait FrontendService extends HttpService with HttpsDirectives with Logging {

  def scheme: String

  def hostname: String

  def captcha_pkey: String

  def bip70RequestBuilder: BIP70RequestBuilder = BIP70RequestBuilder.default

  val customHeaders = List(
    HttpHeaders.RawHeader("Access-Control-Allow-Origin", "*"),
    HttpHeaders.RawHeader("Access-Control-Allow-Headers", "Content-Type"),
    HttpHeaders.RawHeader("Access-Control-Allow-Methods", "PUT, GET, POST, DELETE, OPTIONS"),
    HttpHeaders.RawHeader("Cache-control", "public, no-store, max-age=0"),
    HttpHeaders.RawHeader("Access-Control-Allow-Headers", "x-requested-with")
  )

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  case class PendingRequest(srcEmail: String, tgtEmail: String, amount: Double, address: String, description: String)

  val pendingRequests = new ConcurrentHashMap[String, PendingRequest]()

  val decimalFormatter = new java.text.DecimalFormat("#.########")

  val route =
    logRequestResponse(showErrorResponses _) {
      respondWithHeaders(customHeaders) {
        handleRejections(RejectionHandler.Default) {
          handleExceptions(myExceptionHandler) {
            path("ruok") {
              complete {
                "imok"
              }
            } ~
              enforceHttpsIf(scheme == "https") {
                get {
                  path("request" / Segment) {
                    (requestId) =>
                      validate(pendingRequests.containsKey(requestId), "Unknown request id.") {
                        val pendingRequest = pendingRequests.get(requestId)
                        import pendingRequest._
                        logger.info(s"generating bip70 request for request $pendingRequest")
                        respondWithMediaType(MediaType.custom("application/bitcoin-paymentrequest")) {
                          complete {
                            val (network, script) = Address.decode(address) match {
                              case (Address.LivenetPubkeyVersion, bytes) => ("main", Script.write(OP_DUP :: OP_HASH160 :: OP_PUSHDATA(bytes) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil))
                              case (Address.TestnetPubkeyVersion, bytes) => ("test", Script.write(OP_DUP :: OP_HASH160 :: OP_PUSHDATA(bytes) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil))
                              case (Address.LivenetScriptVersion, bytes) => ("main", Script.write(OP_HASH160 :: OP_PUSHDATA(bytes) :: OP_EQUAL :: Nil))
                              case (Address.TestnetScriptVersion, bytes) => ("test", Script.write(OP_HASH160 :: OP_PUSHDATA(bytes) :: OP_EQUAL :: Nil))
                            }
                            HttpResponse(entity = HttpEntity(bip70RequestBuilder.createBIP70Request(
                              satoshiAmount = (amount * 100000000).toLong,
                              script = script,
                              memo = s"""From $srcEmail (verified): $description""",
                              network = network).toByteArray))
                          }
                        }
                      }
                  } ~
                    path("confirm" / Segment) {
                      (requestId) =>
                        validate(pendingRequests.containsKey(requestId), "Unknown request id.") {
                          val pendingRequest = pendingRequests.get(requestId)
                          import pendingRequest._
                          logger.info(s"received confirmation for request $pendingRequest")
                          respondWithMediaType(MediaTypes.`text/html`) {
                            // XML is marshalled to `text/xml` by default, so we simply override here
                            complete {
                              EmailSender.send(
                                from = "noreply@pay2email.net",
                                to = tgtEmail,
                                subject = s"Bitcoin payment request from $srcEmail",
                                htmlBody = HtmlGenerator.generate("emails/request.tpl.html", Map("scheme" -> scheme, "host" -> hostname, "srcEmail" -> srcEmail, "tgtEmail" -> tgtEmail, "requestId" -> requestId).mapValues(_.asInstanceOf[AnyRef])))
                              HttpResponse(entity = HttpEntity(HtmlGenerator.generate("sent.tpl.html", Map("scheme" -> scheme, "host" -> hostname, "tgtEmail" -> tgtEmail))))
                            }
                          }
                        }
                    } ~
                    path("open" / Segment) {
                      (requestId) =>
                        validate(pendingRequests.containsKey(requestId), "Unknown request id.") {
                          val pendingRequest = pendingRequests.get(requestId)
                          import pendingRequest._
                          logger.info(s"user is opening request $pendingRequest")
                          respondWithMediaType(MediaTypes.`text/html`) {
                            // XML is marshalled to `text/xml` by default, so we simply override here
                            complete {
                              HttpResponse(entity = HttpEntity(HtmlGenerator.generate("open.tpl.html", Map("scheme" -> scheme, "host" -> hostname, "amount" -> decimalFormatter.format(amount), "address" -> address, "requestId" -> requestId))))
                            }
                          }
                        }
                    } ~ path("") {
                    getFromResource("web/index.html")
                  } ~ getFromResourceDirectory("web")
                } ~ post {
                  path("send") {
                    clientIP {
                      ip =>
                        formFields('srcEmail, 'tgtEmail, 'amount.as[Double], 'address, 'description, 'recaptcha_challenge_field, 'recaptcha_response_field) {
                          (srcEmail, tgtEmail, amount, address, description, challenge, response) =>
                            onComplete(CaptchaVerifier.verify(captcha_pkey, ip.toOption.map(_.toString).getOrElse("127.0.0.1"), challenge, response)) {
                              captcha_result =>
                                validate(captcha_result.isSuccess && captcha_result.get, "Captcha verification failed.") {
                                  val decodedAddress = Try(Address.decode(address))
                                  validate(decodedAddress.isInstanceOf[Success[(Byte, Array[Byte])]], "Invalid bitcoin address.") {
                                    validate(List(Address.TestnetPubkeyVersion, Address.TestnetScriptVersion).contains(decodedAddress.asInstanceOf[Success[(Byte, Array[Byte])]].get._1), "Only testnet is supported for now.") {
                                      validate(description.size <= 200, "Description field is too large.") {
                                        complete {
                                          logger.info(s"creating payment request from $srcEmail to $tgtEmail, amount=$amount, address=$address, description=$description")
                                          logger.info(s"requestCount=${pendingRequests.size()} in memory")
                                          val requestId = UUID.randomUUID().toString
                                          pendingRequests.put(requestId, PendingRequest(srcEmail, tgtEmail, amount, address, description))
                                          EmailSender.send(
                                            from = "noreply@pay2email.net",
                                            to = srcEmail, // this is a verification email
                                            subject = s"Email verification from pay2email.net",
                                            htmlBody = HtmlGenerator.generate("emails/confirm.tpl.html", Map("scheme" -> scheme, "scheme" -> scheme, "host" -> hostname, "srcEmail" -> srcEmail, "tgtEmail" -> tgtEmail, "amount" -> decimalFormatter.format(amount), "address" -> address, "description" -> description, "requestId" -> requestId).mapValues(_.asInstanceOf[AnyRef])))
                                          HttpResponse(StatusCodes.SeeOther, entity = HttpEntity.Empty, headers = HttpHeaders.RawHeader("Location", "thanks.html") +: customHeaders)
                                        }
                                      }
                                    }
                                  }
                                }
                            }
                        }
                    }
                  }
                }
              }
          }
        }
      }
    }

  def myExceptionHandler(implicit log: LoggingContext) = ExceptionHandler {
    case t: Throwable => ctx => loggedFailureResponse(ctx, t, StatusCodes.InternalServerError)
  }

  private def loggedFailureResponse(ctx: RequestContext,
                                    thrown: Throwable,
                                    code: StatusCode)
                                   (implicit log: LoggingContext) {
    logger.error(s"error while processing request ${ctx.request}, reason: ${Option(thrown.getMessage).getOrElse("unknown")}, stack trace: ", thrown)
    // returning empty body for security purposes
    ctx.complete(HttpResponse(code, HttpEntity.Empty))
  }

  import akka.event.Logging._
  import spray.http.StatusCodes._

  def showErrorResponses(request: HttpRequest): Any ⇒ Option[LogEntry] = {
    case HttpResponse(OK | NotModified | PartialContent | TemporaryRedirect | SeeOther, _, _, _) ⇒ None
    case response: HttpResponse => Some(LogEntry(s"${response.status}: $request -> $response", ErrorLevel))
    case _ ⇒ None
  }
}

import spray.http.HttpHeaders._
import spray.http.StatusCodes._
import spray.routing.Directives._
import spray.routing._

trait HttpsDirectives {

  import fr.acinq.pay2email.HttpsDirectives._

  def enforceHttpsIf(yes: => Boolean): Directive0 = {
    if (yes) enforceHttps
    else pass
  }

  def enforceHttps: Directive0 = {
    respondWithHeader(StrictTransportSecurity) &
      extract(isHttpsRequest).flatMap(
        if (_) pass
        else redirectToHttps
      )
  }

  def redirectToHttps: Directive0 = {
    requestUri.flatMap { uri =>
      redirect(uri.copy(scheme = "https"), MovedPermanently)
    }
  }
}

object HttpsDirectives {
  /** Hardcoded max-age of one year (31536000 seconds) for now. */
  val StrictTransportSecurity = RawHeader("Strict-Transport-Security", "max-age=31536000")

  val isHttpsRequest: RequestContext => Boolean = { ctx =>
    ctx.request.uri.scheme == "https" || ctx.request.headers.exists(h => h.is("x-forwarded-proto") && h.value == "https")
  }
}