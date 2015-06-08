package com.nrinaudo.fetch

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class ValueWriterSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  describe("ValueWriter") {
    it("should sequence non-empty lists properly") {
      forAll(nonEmptyListOf(arbitrary[Int])) { values =>
        ValueWriter.sequence(values) should be(Some(values.map(_.toString)))
      }
    }

    it("should sequence empty lists properly") {
      ValueWriter.sequence(Nil: List[Int]) should be(None)
    }


    // TODO: test sequence for objects that fail to serialize
  }
}