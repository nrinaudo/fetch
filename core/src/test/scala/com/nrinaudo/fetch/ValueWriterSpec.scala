package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

class ValueWriterSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit val Ints    = ValueFormat.intParam
  implicit val Strings = Headers.stringHeader

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