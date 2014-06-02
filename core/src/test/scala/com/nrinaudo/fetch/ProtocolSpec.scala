package com.nrinaudo.fetch

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object ProtocolSpec {
  /** Generates a random supported protocol (http or https). */
  def protocol: Gen[Protocol] = Gen.oneOf(Protocol.Http, Protocol.Https)

  /** Generates invalid protocol names. */
  def invalidProtocol: Gen[String] = Arbitrary.arbitrary[String].suchThat(s => s != Protocol.Http.name && s != Protocol.Https.name)
}

class ProtocolSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ProtocolSpec._

  describe("The Protocol singleton object") {
    it("should parse valid names") {
      forAll(protocol) { protocol => Protocol.parse(protocol.name) should be(Some(protocol)) }
    }

    it("should not parse invalid protocol names") {
      forAll(invalidProtocol) { str => Protocol.parse(str) should be(None) }
    }
  }

  describe("A Protocol instance") {
    it("should serialize to itself") {
      forAll(protocol) { protocol => Protocol.parse(protocol.toString) should be(Some(protocol)) }
    }

    it("should generate URLs correctly") {
      forAll(protocol, UrlSpec.host) { (protocol, host) =>
        (protocol :/ host).host should be(host)
        protocol.host(host).host should be(host)
      }
    }
  }
}