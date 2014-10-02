package fr.acinq.pay2email

import java.io._
import java.security.PrivateKey
import java.security.cert.CertificateFactory
import java.security.cert.X509Certificate

import fr.acinq.bitcoin.PaymentProtocol
import com.amazonaws.services.s3.AmazonS3Client
import com.google.common.io.Resources
import com.google.protobuf.ByteString
import com.typesafe.config.ConfigFactory
import org.bitcoin.protocols.payments.Protos.{PaymentRequest, Output, PaymentDetails}
import org.bouncycastle.asn1.pkcs.PrivateKeyInfo
import org.bouncycastle.openssl.{PEMEncryptedKeyPair, PEMParser}
import org.bouncycastle.openssl.jcajce.{JcePEMDecryptorProviderBuilder, JcaPEMKeyConverter}

import scala.compat.Platform

object BIP70RequestBuilder {
  val factory = CertificateFactory.getInstance("X.509")

  lazy val default = makeDefaultBuilder

  def makeDefaultBuilder: BIP70RequestBuilder = {
    val intermediate = readCertificateFromResources("ssl/GandiStandardSSLCA.pem")
    val cert = readCertificateFromResources("ssl/pay2email.net.crt")
    val key = {
      val config = ConfigFactory.load().getConfig("app")
      val s3 = new AmazonS3Client()
      val obj = s3.getObject(config.getString("ssl-key.location.bucket"), config.getString("ssl-key.location.path"))
      val in = obj.getObjectContent()
      try {
        readPrivateKey(in, Some(config.getString("ssl-key.password")))
      } finally {
        in.close()
      }
    }
    new BIP70RequestBuilder(key, Seq(cert, intermediate))
  }

  def readCertificateFromResources(path: String): X509Certificate = {
    val in = Resources.asByteSource(Resources.getResource(path)).openStream()
    try {
      factory.generateCertificate(in).asInstanceOf[X509Certificate]
    } finally {
      in.close()
    }
  }

  def readPrivateKey(input: InputStream, password: Option[String] = None): PrivateKey = {
    val converter = new JcaPEMKeyConverter()
    val parser = new PEMParser(new InputStreamReader(input))
    val pem = parser.readObject()
    pem match {
      case keyInfo: PrivateKeyInfo => converter.getPrivateKey(keyInfo)
      case encKeyPair: PEMEncryptedKeyPair if password.isDefined =>
        val decKeyPair = encKeyPair.decryptKeyPair(new JcePEMDecryptorProviderBuilder().build(password.get.toCharArray))
        converter.getPrivateKey(decKeyPair.getPrivateKeyInfo)
    }
  }
}

class BIP70RequestBuilder(key: PrivateKey, certificates: Seq[X509Certificate]) {
  def createBIP70Request(satoshiAmount: Long, script: Array[Byte], memo: String) = {
    val details = PaymentDetails.newBuilder()
      .addOutputs(Output.newBuilder().setAmount(satoshiAmount).setScript(ByteString.copyFrom(script)))
      .setMemo(memo)
      .setTime(Platform.currentTime)

    val request = PaymentRequest.newBuilder()
      .setPaymentDetailsVersion(1)
      .setSerializedPaymentDetails(details.build().toByteString)
      .build

    PaymentProtocol.sign(request, certificates, key)
  }
}
