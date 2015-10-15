package fetch

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import fetch.Generators._

class ETagSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("The ETag companion object") {
    it("should parse valid etags") {
      forAll { etag: ETag => ETag.parse(etag.toString) should be(Some(etag)) }
    }

    it("should not parse invalid etags") {
      forAll(invalidEtag) { str => ETag.parse(str) should be(None) }
    }
  }

  describe("An ETag instance") {
    it("should have the correct weak flag") {
      forAll { etag: ETag =>
        etag match {
          case e@ETag.Weak(_) => e.isWeak should be(true)
          case e@ETag.Strong(_) => e.isWeak should be(false)
        }
      }
    }
  }
}