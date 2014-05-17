package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import java.net.URI

object UrlSpec {
  def domainSeg = for {
    first   <- alphaChar
    length  <- choose(1, 10)
    content <- listOfN(length, alphaLowerChar)
  } yield (first :: content).mkString

  /** Generates a valid host. */
  def host = for {
    name <- domainSeg
    ext  <- oneOf("com", "fr", "es", "it", "co.uk", "co.jp", "io")
  } yield name + "." + ext

  /** Generates a valid port. */
  def port = choose(1, 65535)

  /** Generates a valid path. */
  def path = for {
    count <- choose(0, 5)
    path <- listOfN(count, arbitrary[String].suchThat(!_.isEmpty))
  } yield path

  def ref = oneOf(true, false) flatMap {b =>
    if(b) None
    else  arbitrary[String].map(Some(_))
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

    it("should correctly transform to instances of URI") {
      forAll(url) {url =>
        url.toURI should be(new URI(url.toString))
      }
    }

    it("should serialize to itself") {
      forAll(url) { url =>
        Url(url.toString) should be(url)
      }
    }
  }
}
