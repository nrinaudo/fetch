package fetch

import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import fetch.Generators._

class ProtocolSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
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
      forAll(arbitrary[Protocol], host) { (protocol, host) =>
        protocol.host(host).host should be(host)
      }
    }
  }
}