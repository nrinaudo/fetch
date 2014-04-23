package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

object ByteRangeSpec {
  def boundary = Gen.choose(0, 1000)
  def negBoundary = Gen.choose(-1000, -1)

  def boundaries = for {
    from <- Gen.choose(0, 1000)
    to   <- Gen.choose(from, from + 1000)
  } yield (from, to)
}

/** Tests the [[ByteRange]] class. */
class ByteRangeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ByteRangeSpec._



  describe("A byte range") {
    // - Failure cases -------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should refuse instances where both boundaries are undefined") {
      intercept[IllegalArgumentException] {ByteRange(None, None)}
    }

    it("should refuse negative values for its lower boundary") {
      forAll(negBoundary, Gen.option(boundary)) { (from, to) =>
        intercept[IllegalArgumentException] {ByteRange(Some(from), to)}
      }
    }

    it("should refuse negative values for its upper boundary") {
      forAll(Gen.option(boundary), negBoundary) { (from, to) =>
        intercept[IllegalArgumentException] {ByteRange(from, Some(to))}
      }
    }

    it("should refuse instances where the upper boundary is smaller than the lower one") {
      forAll(boundary, boundary) { (from, to) =>
        intercept[IllegalArgumentException] {ByteRange(Some(math.max(from, to)), Some(math.min(from, to)))}
      }
    }


    // - Serialization -------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should serialize as -[to] when no lower boundary is present") {
      forAll(boundary) { to =>
        ByteRange(None, Some(to)).toString should be("-" + to)
      }
    }

    it("should serialize as [from]- when no upper boundary is present") {
      forAll(boundary) { from =>
        ByteRange(Some(from), None).toString should be(from + "-")
      }
    }

    it("should serialize as [from]-[to] when both boundaries are present") {
      forAll(boundaries) { case (from, to) =>
        ByteRange(Some(from), Some(to)).toString should be(from + "-" + to)
      }
    }
  }
}
