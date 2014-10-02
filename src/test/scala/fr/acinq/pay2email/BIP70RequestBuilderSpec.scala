package fr.acinq.pay2email

import java.security.KeyStore
import java.security.cert.CertificateFactory

import fr.acinq.bitcoin.PaymentProtocol
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BIP70RequestBuilderSpec extends FlatSpec {
  "BIP70RequestBuilder" should "generate valid BIP70 payment requests" in {
    val keystore = KeyStore.getInstance("JKS")
    keystore.load(classOf[BIP70RequestBuilderSpec].getResourceAsStream("/cacerts"), null)
    val factory = CertificateFactory.getInstance("X.509")
    val cacert = BIP70RequestBuilder.readCertificateFromResources("cacert.pem")
    val servercert = BIP70RequestBuilder.readCertificateFromResources("servercert.pem")
    val key = BIP70RequestBuilder.readPrivateKey(classOf[BIP70RequestBuilderSpec].getResourceAsStream("/serverkey.pem"))

    keystore.setCertificateEntry("foo", cacert)
    val builder = new BIP70RequestBuilder(key, Seq(servercert))
    val request = builder.createBIP70Request(1, Array(), "foo")
    val (name, publicKey, trustAnchor) = PaymentProtocol.verifySignature(request, keystore)
    assert(name === "Foobar")
  }
}
