package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

class ValueReaderSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  describe("ValueReader") {
    it("should sequence empty lists properly") {
      ValueReader.sequence[Int](Nil) should be(Some(Nil))
    }

    it("should sequence successes properly") {
      forAll(Gen.listOf(Arbitrary.arbitrary[Int])) { values =>
        ValueReader.sequence[Int](values.map(_.toString)) should be(Some(values))
      }
    }

    def brokenSequence = for {
      ints <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Int])
      pos  <- Gen.choose(0, ints.length)
    } yield {
      val (h, t) = ints.map(_.toString).splitAt(pos)
      h ::: ("test" :: t)
    }

    it("should sequence failures properly") {
      forAll(brokenSequence) { values => ValueReader.sequence[Int](values) should be(None) }
    }
  }
}
