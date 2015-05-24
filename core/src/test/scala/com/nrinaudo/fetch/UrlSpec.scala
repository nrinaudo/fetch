package com.nrinaudo.fetch

import org.scalacheck.{Gen, Arbitrary}
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import QueryStringSpec._
import ProtocolSpec._

object UrlSpec {
  def domainSeg: Gen[String] = for {
    first   <- alphaChar
    length  <- choose(1, 10)
    content <- listOfN(length, alphaLowerChar)
  } yield (first :: content).mkString

  /** Generates a valid host. */
  def host: Gen[String] = for {
    name <- domainSeg
    ext  <- oneOf("com", "fr", "es", "it", "co.uk", "co.jp", "io")
  } yield name + "." + ext

  /** Generates a valid port. */
  def port: Gen[Int] = choose(1, 65535)

  def segment: Gen[String] = arbitrary[String].suchThat(!_.isEmpty)

  /** Generates a valid path. */
  def path: Gen[List[String]] = for {
    count <- choose(0, 5)
    path <- listOfN(count, segment)
  } yield path

  def fragment: Gen[Option[String]] = oneOf(true, false) flatMap {b =>
    if(b) None
    else  arbitrary[String].map(Some(_))
  }

  implicit val url: Arbitrary[Url] = Arbitrary {
    for {
      pr <- arbitrary[Protocol]
      h  <- host
      p  <- port
      s  <- path
      q  <- arbitrary[QueryString]
      r  <- fragment
    } yield Url(pr, h, p, s, q, r)
  }
}

class UrlSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import UrlSpec._

  describe("An Url") {
    it("should correctly change protocol") {
      forAll { (url: Url, protocol: Protocol) =>
        url.protocol(protocol).protocol should be(protocol)
      }
    }

    it("should correctly change host") {
      forAll(arbitrary[Url], host) { (url, host) =>
        url.host(host).host should be(host)
      }
    }

    it("should correctly change port") {
      forAll(arbitrary[Url], port) { (url, port) =>
        url.port(port).port should be(port)
      }
    }

    it("should correctly change path") {
      forAll(arbitrary[Url], path) { (url, path) =>
        url.path(path).path should be(path)
      }
    }

    it("should correctly append segments to its path") {
      forAll(arbitrary[Url], segment) { (url, segment) =>
        url.addSegment(segment).path should be(url.path :+ segment)
        (url / segment).path should be(url.path :+ segment)
      }
    }

    it("should correctly change query string") {
      forAll { (url: Url, query: QueryString) =>
        url.query(query).query should be(query)
        (url ? query).query should be(query)
      }
    }

    it("should correctly change fragment") {
      forAll(arbitrary[Url], fragment) { (url, fragment) =>
        url.fragment(fragment).fragment should be(fragment)
      }
    }

    it("should correctly append parameters to the query string") {
      import QueryString._

      forAll(arbitrary[Url], queryParam) { (url, param) =>
        url.param(param._1, param._2: _*).query should be(url.query.set(param._1, param._2: _*))
      }
    }

    it("should ignore default ports") {
      forAll(arbitrary[Protocol], host) {(protocol, host) =>
        Url(protocol, host, protocol.defaultPort).toString should be(protocol.name + "://" + host + "/")
      }
    }

    it("should include non-default ports") {
      forAll(host, port.suchThat(_ != Protocol.Http.defaultPort)) { (host, port) =>
        Protocol.Http.host(host).port(port).toString should be("http://" + host + ":" + port + "/")
      }
    }

    it("should correctly parse instances of URI") {
      forAll { url: Url =>
        Url.fromUri(url.toURI) should be(Some(url))

        // Makes sure that we test URLs with the default port, as while the generator will not generate this often,
        // it's the default case in real life.
        val url2 = url.port(url.protocol.defaultPort)
        Url.fromUri(url2.toURI) should be(Some(url2))
      }
    }

    it("should correctly parse valid instances of String") {
      forAll { url: Url =>
        Url.parse(url.toString) should be(Some(url))

        // Makes sure that we test URLs with the default port, as while the generator will not generate this often,
        // it's the default case in real life.
        val url2 = url.port(url.protocol.defaultPort)
        Url.parse(url2.toString) should be(Some(url2))
      }
    }
  }
}
