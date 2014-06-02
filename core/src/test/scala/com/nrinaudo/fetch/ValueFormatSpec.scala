package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary
import scala.Some

class ValueFormatSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  def cycle[A](value: A, format: ValueFormat[A]) {
    format.write(value).flatMap(format.read) should be(Some(value))
  }

  describe("ValueFormat") {
    it("should have a working default Double implementation") {
      forAll(Arbitrary.arbitrary[Double]) { value => cycle(value, ValueFormat.Doubles)}
    }

    it("should have a working default Long implementation") {
      forAll(Arbitrary.arbitrary[Long]) { value => cycle(value, ValueFormat.Longs)}
    }

    it("should have a working default Short implementation") {
      forAll(Arbitrary.arbitrary[Short]) { value => cycle(value, ValueFormat.Shorts)}
    }

    it("should have a working default Int implementation") {
      forAll(Arbitrary.arbitrary[Int]) { value => cycle(value, ValueFormat.Ints)}
    }

    it("should have a working default Byte implementation") {
      forAll(Arbitrary.arbitrary[Byte]) { value => cycle(value, ValueFormat.Bytes)}
    }

    it("should have a working default Float implementation") {
      forAll(Arbitrary.arbitrary[Float]) { value => cycle(value, ValueFormat.Floats)}
    }

    it("should have a working default Boolean implementation") {
      forAll(Arbitrary.arbitrary[Boolean]) { value => cycle(value, ValueFormat.Booleans)}
    }

    it("should have a working default String implementation") {
      forAll(Arbitrary.arbitrary[String]) { value => cycle(value, ValueFormat.Strings)}
    }
  }
}