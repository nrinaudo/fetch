package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

/** Utilities for testing [[ByteRange]]. */
object ByteRangeSpec {
  /** Generates valid byte range boundaries. */
  def boundary = Gen.choose(0, 1000)

  /** Generates invalid byte range boundaries. */
  def negBoundary = Gen.choose(-1000, -1)

  /** Generate valid byte range boundaries. */
  def boundaries = for {
    from <- Gen.choose(0, 1000)
    to   <- Gen.choose(from, from + 1000)
  } yield (from, to)

  /** Generates valid byte ranges. */
  def byteRange = for {
    (from, to) <- boundaries
    opt        <- Gen.oneOf(0, 1, 2)
  // Possibly not the cleanest generator I've ever written, but it does the job and will not be embarked in any
  // live code.
  } yield ByteRange(if(opt == 1) None else Some(from), if(opt ==2 ) None else Some(to))
}

/** Tests the [[ByteRange]] class. */
class ByteRangeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ByteRangeSpec._



  describe("A byte range") {
    // - Failure cases -------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should refuse instances where both boundaries are undefined") {
      intercept[IllegalArgumentException] {
        ByteRange(None, None)
      }
    }

    it("should refuse negative values for its lower boundary") {
      forAll(negBoundary, Gen.option(boundary)) { (from, to) =>
        intercept[IllegalArgumentException] {
          ByteRange(Some(from), to)
        }
      }
    }

    it("should refuse negative values for its upper boundary") {
      forAll(Gen.option(boundary), negBoundary) { (from, to) =>
        intercept[IllegalArgumentException] {
          ByteRange(from, Some(to))
        }
      }
    }

    it("should refuse instances where the upper boundary is smaller than the lower one") {
      forAll(boundaries.suchThat {b => b._1 != b._2}) { case (from, to) =>
        intercept[IllegalArgumentException] {
          ByteRange(Some(math.max(from, to)), Some(math.min(from, to)))
        }
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


    // - Parsing ------------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should parse valid instances correctly") {
      forAll(byteRange) { range =>
        val ByteRange(parsed) = range.toString
        parsed should be(range)
      }
    }
  }
}
