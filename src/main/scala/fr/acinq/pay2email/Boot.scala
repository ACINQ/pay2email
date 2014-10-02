package fr.acinq.pay2email

import java.security.cert.CertificateFactory

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import com.amazonaws.services.s3.AmazonS3Client
import com.typesafe.config.ConfigFactory
import spray.can.Http

object Boot extends App {

  val conf = ConfigFactory.load().getConfig("app")

  // we need an ActorSystem to host our application in
  implicit val system = ActorSystem("system")

  // create and start our service actor
  val service = system.actorOf(Props(new FrontendServiceActor {
    override def hostname: String = conf.getString("hostname")

    override def captcha_pkey: String = conf.getString("captcha-pkey")

  }), "frontend-service")

  // start a new HTTP server on port 8080 with our service actor as the handler
  IO(Http) ! Http.Bind(service, conf.getString("http.ip"), port = conf.getInt("http.port"))
}
