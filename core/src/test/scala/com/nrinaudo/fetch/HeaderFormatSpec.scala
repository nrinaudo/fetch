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
  def cycle[T](value: T)(implicit reader: ValueReader[T], writer: ValueWriter[T]) = reader.read(writer.write(value).get)

}

trait HeaderFormatSpec[T] extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  def illegalT: Gen[String]
  implicit def arbT: Arbitrary[T]
  implicit def reader: ValueReader[T]
  implicit def writer: ValueWriter[T]

  def validate[F: ValueReader: ValueWriter](value: F) = HeaderFormatSpec.cycle(value) should be(Some(value))

  it("should correctly serialize and parse legal values") {
    forAll { value: T => validate(value) }
  }

  it("should refuse illegal values") {
    forAll(illegalT) { str => reader.read(str).isEmpty should be(true) }
  }
}

trait HeaderFormatSpecWithList[T] extends HeaderFormatSpec[T] {
  implicit def seqReader: ValueReader[Seq[T]] = compositeReader
  implicit def seqWriter: ValueWriter[Seq[T]] = compositeWriter

  it("should correctly serialize and parse lists of legal values") {
    forAll(nonEmptyListOf(arbitrary[T])) { ts => validate(ts: Seq[T]) }
  }
}

class DateFormatSpec extends HeaderFormatSpec[Date] {
  override val illegalT = illegalDate
  override val arbT     = arbDate
  override val reader   = HttpDate
  override val writer   = HttpDate
}

class LanguageFormatSpec extends HeaderFormatSpecWithList[Language] {
  override val illegalT = illegalLanguage
  override val arbT     = arbLanguage
  override val reader   = implicitly[ValueReader[Language]]
  override val writer   = implicitly[ValueWriter[Language]]
}

class CharsetFormatSpec extends HeaderFormatSpecWithList[Charset] {
  override val illegalT = illegalCharset
  override val arbT     = arbCharset
  override val reader   = implicitly[ValueReader[Charset]]
  override val writer   = implicitly[ValueWriter[Charset]]
}

class MediaTypeFormatSpec extends HeaderFormatSpec[MediaType] {
  override val illegalT = illegalMediaType
  override val arbT     = arbMediaType
  override val reader   = implicitly[ValueReader[MediaType]]
  override val writer   = implicitly[ValueWriter[MediaType]]
}

class EncodingFormatSpec extends HeaderFormatSpecWithList[Encoding] {
  override val illegalT = illegalEncoding
  override val arbT     = arbEncoding
  override val reader   = implicitly[ValueReader[Encoding]]
  override val writer   = implicitly[ValueWriter[Encoding]]
}

class ByteRangeFormatSpec extends HeaderFormatSpec[ByteRange] {
  override val illegalT = illegalRange
  override val arbT     = arbByteRange
  override val reader   = implicitly[ValueReader[ByteRange]]
  override val writer   = implicitly[ValueWriter[ByteRange]]
}

class ByteRangesFormatSpec extends HeaderFormatSpec[Seq[ByteRange]] {
  override val illegalT = illegalRanges
  override val arbT     = Arbitrary(nonEmptyListOf(arbByteRange.arbitrary): Gen[Seq[ByteRange]])
  override val reader   = implicitly[ValueReader[Seq[ByteRange]]]
  override val writer   = implicitly[ValueWriter[Seq[ByteRange]]]
}

class MethodFormatSpec extends HeaderFormatSpecWithList[Method] {
  override val illegalT = illegalMethod
  override val arbT     = arbMethod
  override val reader   = implicitly[ValueReader[Method]]
  override val writer   = implicitly[ValueWriter[Method]]
}
