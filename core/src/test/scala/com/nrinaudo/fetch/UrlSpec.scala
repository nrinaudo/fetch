package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import QueryStringSpec._

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

  def segment = arbitrary[String].suchThat(!_.isEmpty)

  /** Generates a valid path. */
  def path = for {
    count <- choose(0, 5)
    path <- listOfN(count, segment)
  } yield path

  def fragment = oneOf(true, false) flatMap {b =>
    if(b) None
    else  arbitrary[String].map(Some(_))
  }

  def url = for {
    pr <- ProtocolSpec.protocol
    h  <- host
    p  <- port
    s  <- path
    q  <- query
    r  <- fragment
  } yield Url(pr, h, p, s, q, r)
}

class UrlSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ProtocolSpec._
  import UrlSpec._

  describe("An Url") {
    it("should correctly change protocol") {
      forAll(url, protocol) { (url, protocol) =>
        url.protocol(protocol).protocol should be(protocol)
      }
    }

    it("should correctly change host") {
      forAll(url, host) { (url, host) =>
        url.host(host).host should be(host)
      }
    }

    it("should correctly change port") {
      forAll(url, port) { (url, port) =>
        url.port(port).port should be(port)
      }
    }

    it("should correctly change path") {
      forAll(url, path) { (url, path) =>
        url.path(path).path should be(path)
      }
    }

    it("should correctly append segments to its path") {
      forAll(url, segment) { (url, segment) =>
        url.addSegment(segment).path should be(url.path :+ segment)
        (url / segment).path should be(url.path :+ segment)
      }
    }

    it("should correctly change query string") {
      forAll(url, query) { (url, query) =>
        url.query(query).query should be(query)
        (url ? query).query should be(query)
      }
    }

    it("should correctly change fragment") {
      forAll(url, fragment) { (url, fragment) =>
        url.fragment(fragment).fragment should be(fragment)
      }
    }

    it("should correctly append parameters to the query string") {
      import QueryString._

      forAll(url, queryParam) { (url, param) =>
        url.param(param._1, param._2: _*).query should be(url.query.set(param._1, param._2: _*))
      }
    }

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

    it("should correctly apply and unapply instances of URI") {
      forAll(url) { url =>
        Url(url.toURI) should be(url)
        Url.unapply(url.toURI) should be(Some(url))

        // Makes sure that we test URLs with the default port, as while the generator will not generate this often,
        // it's the default case in real life.
        val url2 = url.port(url.protocol.defaultPort)
        Url(url2.toURI) should be(url2)
        Url.unapply(url2.toURI) should be(Some(url2))
      }
    }

    it("should correctly apply and unapply valid instances of String") {
      forAll(url) { url =>
        Url(url.toString) should be(url)
        Url.unapply(url.toString) should be(Some(url))

        // Makes sure that we test URLs with the default port, as while the generator will not generate this often,
        // it's the default case in real life.
        val url2 = url.port(url.protocol.defaultPort)
        Url(url2.toString) should be(url2)
        Url.unapply(url2.toString) should be(Some(url2))
      }
    }
  }
}
