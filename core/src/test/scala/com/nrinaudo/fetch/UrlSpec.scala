package com.nrinaudo.fetch

import com.nrinaudo.fetch.Generators._
import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class UrlSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
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
