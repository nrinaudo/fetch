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
    it("should unapply on valid names") {
      forAll(protocol) { protocol => Protocol.unapply(protocol.name) should be(Some(protocol)) }
    }

    it("should apply on valid names") {
      forAll(protocol) { protocol => Protocol(protocol.name) should be(protocol) }
    }

    it("should refuse to unapply on invalid protocol names") {
      forAll(invalidProtocol) { str => Protocol.unapply(str) should be(None) }
    }

    it("should fail to apply on invalid protocol names") {
      forAll(invalidProtocol) { str => intercept[IllegalArgumentException] {Protocol(str)} }
    }
  }

  describe("A Protocol instance") {
    it("should serialize to itself") {
      forAll(protocol) { protocol => Protocol(protocol.toString) should be(protocol) }
    }

    it("should generate URLs correctly") {
      forAll(protocol, UrlSpec.host) { (protocol, host) =>
        (protocol :/ host).host should be(host)
        protocol.host(host).host should be(host)
      }
    }
  }
}