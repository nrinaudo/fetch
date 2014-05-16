package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.util.Success
import org.scalacheck.{Arbitrary, Gen}

class ValueReaderSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit val Ints = ValueFormat.Ints

  describe("ValueReader") {
    it("should sequence empty lists properly") {
      ValueReader.sequence[Int](Nil) should be(Success(Nil))
    }

    it("should sequence successes properly") {
      forAll(Gen.listOf(Arbitrary.arbitrary[Int])) { values =>
        ValueReader.sequence[Int](values.map(_.toString)) should be(Success(values))
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
      forAll(brokenSequence) { values =>
        val failure = ValueReader.sequence[Int](values)

        failure.isFailure should be(true)
        intercept[NumberFormatException](failure.get)
      }
    }
  }
}
