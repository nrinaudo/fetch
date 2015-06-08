package com.nrinaudo.fetch

import java.nio.charset.Charset
import java.util.Date

import com.nrinaudo.fetch.Generators._
import com.nrinaudo.fetch.Headers._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

object HeaderFormatSpec {
  // TODO: rename this, cycle is a horrible name
  def cycle[T](format: ValueFormat[T], value: T) = format.read(format.write(value).get)
}

trait HeaderFormatSpec[T] extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  def illegalT: Gen[String]
  implicit def arbT: Arbitrary[T]
  implicit def formatT: ValueFormat[T]

  def validate[F: ValueFormat](value: F) = HeaderFormatSpec.cycle(implicitly[ValueFormat[F]], value) should be(Some(value))

  it("should correctly serialize and parse legal values") {
    forAll { value: T => validate(value) }
  }

  it("should refuse illegal values") {
    forAll(illegalT) { str => formatT.read(str).isEmpty should be(true) }
  }
}

trait HeaderFormatSpecWithList[T] extends HeaderFormatSpec[T] {
  it("should correctly serialize and parse lists of legal values") {
    forAll(nonEmptyListOf(arbitrary[T])) { ts => validate(ts: Seq[T])(compositeFormat[T]) }
  }
}

class DateFormatSpec extends HeaderFormatSpec[Date] {
  override val illegalT = illegalDate

  override implicit val arbT    = arbDate
  override implicit val formatT = dateHeader
}

class LanguageFormatSpec extends HeaderFormatSpecWithList[Language] {
  override val illegalT = illegalLanguage

  override implicit val arbT    = arbLanguage
  override implicit val formatT = languageHeader
}

class CharsetFormatSpec extends HeaderFormatSpecWithList[Charset] {
  override val illegalT = illegalCharset

  override implicit val arbT    = arbCharset
  override implicit val formatT = ValueFormat(ValueReader.Charset, ValueWriter.Charset)
}

class MediaTypeFormatSpec extends HeaderFormatSpec[MediaType] {
  override val illegalT = illegalMediaType

  override implicit val arbT    = arbMediaType
  override implicit val formatT = mediaTypeHeader
}

class EncodingFormatSpec extends HeaderFormatSpecWithList[Encoding] {
  override val illegalT = illegalEncoding

  override implicit val arbT    = arbEncoding
  override implicit val formatT = encodingHeader
}

class ByteRangeFormatSpec extends HeaderFormatSpec[ByteRange] {
  override val illegalT = illegalRange

  override implicit val arbT    = arbByteRange
  override implicit val formatT = byteRangeHeader
}

class ByteRangesFormatSpec extends HeaderFormatSpec[Seq[ByteRange]] {
  override val illegalT = illegalRanges

  override implicit val arbT: Arbitrary[Seq[ByteRange]] = Arbitrary(nonEmptyListOf(arbByteRange.arbitrary))
  override implicit val formatT                         = byteRangesHeader
}

class MethodFormatSpec extends HeaderFormatSpecWithList[Method] {
  override val illegalT = illegalMethod

  override implicit val arbT    = arbMethod
  override implicit val formatT = methodHeader
}
