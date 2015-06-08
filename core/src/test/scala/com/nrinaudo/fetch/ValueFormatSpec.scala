package com.nrinaudo.fetch

import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class ValueFormatSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  def cycle[A](value: A)(implicit reader: ValueReader[A], writer: ValueWriter[A]): Unit = {
    writer.write(value).flatMap(reader.read) should be(Some(value))
  }

  describe("ValueFormat") {
    it("should have a working default Double implementation") {
      forAll { value: Double => cycle(value) }
    }

    it("should have a working default Long implementation") {
      forAll { value: Long => cycle(value) }
    }

    it("should have a working default Short implementation") {
      forAll { value: Short => cycle(value) }
    }

    it("should have a working default Int implementation") {
      forAll { value: Int => cycle(value) }
    }

    it("should have a working default Byte implementation") {
      forAll { value: Byte => cycle(value) }
    }

    it("should have a working default Float implementation") {
      forAll { value: Float => cycle(value) }
    }

    it("should have a working default Boolean implementation") {
      forAll { value: Boolean => cycle(value) }
    }

    it("should have a working default String implementation") {
      forAll { value: String => cycle(value) }
    }
  }
}