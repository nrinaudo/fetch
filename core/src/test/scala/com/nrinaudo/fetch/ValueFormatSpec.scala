package com.nrinaudo.fetch

import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class ValueFormatSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  def cycle[A](value: A)(implicit reader: ValueReader[A], writer: ValueWriter[A]): Unit = {
    writer.write(value).flatMap(reader.read) should be(Some(value))
  }

  describe("ValueFormat") {
    it("should have a working default Double implementation") {
      forAll(Arbitrary.arbitrary[Double]) { value => cycle(value) }
    }

    it("should have a working default Long implementation") {
      forAll(Arbitrary.arbitrary[Long]) { value => cycle(value) }
    }

    it("should have a working default Short implementation") {
      forAll(Arbitrary.arbitrary[Short]) { value => cycle(value) }
    }

    it("should have a working default Int implementation") {
      forAll(Arbitrary.arbitrary[Int]) { value => cycle(value) }
    }

    it("should have a working default Byte implementation") {
      forAll(Arbitrary.arbitrary[Byte]) { value => cycle(value) }
    }

    it("should have a working default Float implementation") {
      forAll(Arbitrary.arbitrary[Float]) { value => cycle(value) }
    }

    it("should have a working default Boolean implementation") {
      forAll(Arbitrary.arbitrary[Boolean]) { value => cycle(value) }
    }

    it("should have a working default String implementation") {
      forAll(Arbitrary.arbitrary[String]) { value => cycle(value) }
    }
  }
}