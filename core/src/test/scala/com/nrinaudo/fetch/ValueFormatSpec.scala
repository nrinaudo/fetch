package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Gen, Arbitrary}
import scala.util.Success

class ValueWriterSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit val Ints    = ValueFormat.Ints
  implicit val Strings = Headers.StringFormat

  describe("ValueWriter") {
    it("should sequence non-empty lists properly") {
      forAll(Gen.listOf(Arbitrary.arbitrary[Int]).suchThat(!_.isEmpty)) { values =>
        ValueWriter.sequence(values) should be(Some(values.map(_.toString)))
      }
    }

    it("should sequence empty lists properly") {
      ValueWriter.sequence(Nil: List[Int]) should be(None)
    }

    it("should sequence non-empty lists of empty elements properly") {
      forAll(Gen.choose(1, 10)) { size =>
        ValueWriter.sequence(List.fill(size)("")) should be(None)
      }
    }

    it("should sequence lists with some empty elements properly") {
      forAll(Gen.nonEmptyListOf(Arbitrary.arbitrary[String])) { values =>
        val e1 = values.filter(!_.isEmpty)
        val e2 = if(e1.isEmpty) None else Some(e1)

        ValueWriter.sequence(values) should be(e2)
      }
    }
  }
}

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
