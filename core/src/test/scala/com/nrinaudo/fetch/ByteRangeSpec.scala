package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._

/** Utilities for testing [[ByteRange]]. */
object ByteRangeSpec {
  def illegalRange: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.matches(".*[^0-9-].*"))

  def illegalRanges: Gen[String] = Arbitrary.arbitrary[String].suchThat(!_.startsWith("bytes="))

  /** Generates valid byte range boundaries. */
  def boundary: Gen[Int] = Gen.choose(0, 1000)

  /** Generates invalid byte range boundaries. */
  def negBoundary: Gen[Int] = Gen.choose(-1000, -1)

  /** Generate valid byte range boundaries. */
  def boundaries: Gen[(Int, Int)] = for {
    from <- Gen.choose(0, 1000)
    to   <- Gen.choose(from, from + 1000)
  } yield (from, to)


  implicit val arbPrefixRange: Arbitrary[PrefixRange] = Arbitrary(boundary.map(PrefixRange.apply))
  implicit val arbSuffixRange: Arbitrary[SuffixRange] = Arbitrary(boundary.map(SuffixRange.apply))
  implicit val arbFullRange: Arbitrary[FullRange] = Arbitrary {
    for {
      from <- boundary
      to   <- boundary
    } yield FullRange(math.min(from, to), math.max(from, to))
  }
  implicit val arbByteRange: Arbitrary[ByteRange] =
    Arbitrary(Gen.oneOf(arbitrary[PrefixRange], arbitrary[SuffixRange], arbitrary[FullRange]))

  implicit val byteRanges: Arbitrary[List[ByteRange]] = Arbitrary(HeadersSpec.headers(arbitrary[ByteRange]))
}

/** Tests the [[ByteRange]] class. */
class ByteRangeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ByteRangeSpec._

  describe("A FullRange") {
    it("should refuse negative values for its lower boundary") {
      forAll(negBoundary, boundary) { (from, to) =>
        intercept[IllegalArgumentException] { FullRange(from, to) }
        ()
      }
    }

    it("should refuse negative values for its upper boundary") {
      forAll(boundary, negBoundary) { (from, to) =>
        intercept[IllegalArgumentException] { FullRange(from, to) }
        ()
      }
    }

    it("should refuse instances where the upper boundary is smaller than the lower one") {
      forAll(boundaries.suchThat {b => b._1 != b._2}) { case (from, to) =>
        intercept[IllegalArgumentException] { FullRange(math.max(from, to), math.min(from, to)) }
        ()
      }
    }

    it("should serialize as [from]-[to] when both boundaries are present") {
      forAll(boundaries) { case (from, to) =>
        FullRange(from, to).toString should be(from + "-" + to)
      }
    }
  }

  describe("A PrefixRange") {
    it("should refuse negative values for its value") {
      forAll(negBoundary) { from =>
        intercept[IllegalArgumentException] { PrefixRange(from) }
        ()
      }
    }

    it("should serialize as [from]-") {
      forAll(boundary) { from =>
        PrefixRange(from).toString should be(from + "-")
      }
    }
  }

  describe("A SuffixRange") {
    it("should refuse negative values for its value") {
      forAll(negBoundary) { from =>
        intercept[IllegalArgumentException] { SuffixRange(from) }
        ()
      }
    }

    it("should serialize as -[to]") {
      forAll(boundary) { to => SuffixRange(to).toString should be("-" + to) }
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
