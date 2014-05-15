package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import java.net.URI

object UrlSpec {
  def domainSeg = for {
    first   <- Gen.alphaChar
    length  <- Gen.choose(1, 10)
    content <- Gen.listOfN(length, Gen.alphaLowerChar)
  } yield (first :: content).mkString

  /** Generates a valid host. */
  def host = for {
    name <- domainSeg
    ext  <- Gen.oneOf("com", "fr", "es", "it", "co.uk", "co.jp", "io")
  } yield name + "." + ext

  /** Generates a valid port. */
  def port = Gen.choose(1, 65535)

  /** Generates a valid path. */
  def path = Gen.listOf(Arbitrary.arbitrary[String].suchThat(!_.isEmpty))

  def ref = Gen.oneOf(true, false) flatMap {b =>
    if(b) None
    else  Gen.identifier.map(Some(_))
  }

  def url = for {
    pr <- ProtocolSpec.protocol
    h  <- host
    p  <- port
    s  <- path
    q  <- QueryStringSpec.query
    r  <- ref
  } yield Url(pr, h, p, s, q, r)
}

/**
 * @author Nicolas Rinaudo
 */
class UrlSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ProtocolSpec._
  import UrlSpec._

  describe("An Url") {
    it("should ignore default ports") {
      forAll(protocol, host) {(protocol, host) =>
        Url(protocol, host, protocol.defaultPort).toString should be(protocol.name + "://" + host + "/")
      }
    }

    it("should include non-default ports") {
      forAll(host, port.suchThat(_ != Protocol.Http.defaultPort)) { (host, port) =>
        Protocol.Http.host(host).port(port).toString should be("http://" + host + ":" + port + "/")
      }
    }

    it("should serialize to itself") {
      forAll(url) {url =>
        new URI(url.toString).toString should be(url.toString)
        Url(url.toString) should be(url)
      }
    }
  }
}
