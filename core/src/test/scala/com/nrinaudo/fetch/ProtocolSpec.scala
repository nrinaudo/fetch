package com.nrinaudo.fetch

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object ProtocolSpec {
  /** Generates a random supported protocol (http or https). */
  implicit val protocol: Arbitrary[Protocol] = Arbitrary(Gen.oneOf(Protocol.Http, Protocol.Https))

  /** Generates invalid protocol names. */
  def invalidProtocol: Gen[String] = Arbitrary.arbitrary[String].suchThat(s => s != Protocol.Http.name && s != Protocol.Https.name)
}

class ProtocolSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ProtocolSpec._

  describe("The Protocol singleton object") {
    it("should parse valid names") {
      forAll { protocol: Protocol => Protocol.parse(protocol.name) should be(Some(protocol)) }
    }

    it("should not parse invalid protocol names") {
      forAll(invalidProtocol) { str => Protocol.parse(str) should be(None) }
    }
  }

  describe("A Protocol instance") {
    it("should serialize to itself") {
      forAll { protocol: Protocol => Protocol.parse(protocol.toString) should be(Some(protocol)) }
    }

    it("should unapply as expected") {
      forAll { protocol: Protocol =>
        val Protocol(name, port) = protocol
        name should be(protocol.name)
        port should be(protocol.defaultPort)
      }
    }

    it("should generate URLs correctly") {
      forAll(arbitrary[Protocol], UrlSpec.host) { (protocol, host) =>
        protocol.host(host).host should be(host)
      }
    }
  }
}