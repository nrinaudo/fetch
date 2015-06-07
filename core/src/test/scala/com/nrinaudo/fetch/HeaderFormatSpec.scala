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
  override val illegalT = HeadersSpec.illegalDate

  override implicit val arbT    = HeadersSpec.arbDate
  override implicit val formatT = dateHeader
}

class LanguageFormatSpec extends HeaderFormatSpecWithList[Language] {
  override val illegalT = LanguageSpec.illegalLanguage

  override implicit val arbT    = LanguageSpec.arbLanguage
  override implicit val formatT = languageHeader
}

class CharsetFormatSpec extends HeaderFormatSpecWithList[Charset] {
  override val illegalT = ConnegSpec.illegalCharset

  override implicit val arbT    = ConnegSpec.arbCharset
  override implicit val formatT = charsetHeader
}

class MediaTypeFormatSpec extends HeaderFormatSpec[MediaType] {
  override val illegalT = illegalMediaType

  override implicit val arbT    = arbMediaType
  override implicit val formatT = mediaTypeHeader
}

class EncodingFormatSpec extends HeaderFormatSpecWithList[Encoding] {
  override val illegalT = EncodingSpec.illegalEncoding

  override implicit val arbT    = EncodingSpec.arbEncoding
  override implicit val formatT = encodingHeader
}

class ByteRangeFormatSpec extends HeaderFormatSpec[ByteRange] {
  override val illegalT = ByteRangeSpec.illegalRange

  override implicit val arbT    = ByteRangeSpec.arbByteRange
  override implicit val formatT = byteRangeHeader
}

class ByteRangesFormatSpec extends HeaderFormatSpec[Seq[ByteRange]] {
  override val illegalT = ByteRangeSpec.illegalRanges

  override implicit val arbT: Arbitrary[Seq[ByteRange]] = Arbitrary(nonEmptyListOf(ByteRangeSpec.arbByteRange.arbitrary))
  override implicit val formatT                         = byteRangesHeader
}

class MethodFormatSpec extends HeaderFormatSpecWithList[Method] {
  override val illegalT = MethodSpec.illegalMethod

  override implicit val arbT    = MethodSpec.arbMethod
  override implicit val formatT = methodHeader
}
