package com.nrinaudo.fetch

import com.nrinaudo.fetch.Generators._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

/** Tests the [[ByteRange]] class. */
class ByteRangeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("A FullRange") {
    it("should refuse negative values for its lower boundary") {
      forAll(negRangeBoundary, rangeBoundary) { (from, to) =>
        intercept[IllegalArgumentException] { FullRange(from, to) }
        ()
      }
    }

    it("should refuse negative values for its upper boundary") {
      forAll(rangeBoundary, negRangeBoundary) { (from, to) =>
        intercept[IllegalArgumentException] { FullRange(from, to) }
        ()
      }
    }

    it("should refuse instances where the upper boundary is smaller than the lower one") {
      forAll(rangeBoundaries.suchThat {b => b._1 != b._2}) { case (from, to) =>
        intercept[IllegalArgumentException] { FullRange(math.max(from, to), math.min(from, to)) }
        ()
      }
    }

    it("should serialize as [from]-[to] when both boundaries are present") {
      forAll(rangeBoundaries) { case (from, to) =>
        FullRange(from, to).toString should be(from + "-" + to)
      }
    }
  }

  describe("A PrefixRange") {
    it("should refuse negative values for its value") {
      forAll(negRangeBoundary) { from =>
        intercept[IllegalArgumentException] { PrefixRange(from) }
        ()
      }
    }

    it("should serialize as [from]-") {
      forAll(rangeBoundary) { from =>
        PrefixRange(from).toString should be(from + "-")
      }
    }
  }

  describe("A SuffixRange") {
    it("should refuse negative values for its value") {
      forAll(negRangeBoundary) { from =>
        intercept[IllegalArgumentException] { SuffixRange(from) }
        ()
      }
    }

    it("should serialize as -[to]") {
      forAll(rangeBoundary) { to => SuffixRange(to).toString should be("-" + to) }
    }
  }

  describe("A ByteRange") {
    it("should fail to parse illegal strings") {
      forAll(illegalRange) { range => ByteRange.parse(range).isEmpty should be(true) }
    }

    it("should serialize to itself") {
      forAll { range: ByteRange => ByteRange.parse(range.toString) should be(Some(range)) }
    }
  }
}
