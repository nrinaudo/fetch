package com.nrinaudo.fetch

import java.nio.charset.Charset
import java.util.Date

import com.nrinaudo.fetch.Generators._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

object ValueFormatSpec {
  class FromImplicits[T: ValueReader: ValueWriter: Arbitrary](override val illegalT: Gen[String]) extends WithErrors[T] {
    override val arbT     = implicitly[Arbitrary[T]]
    override val writer   = implicitly[ValueWriter[T]]
    override val reader   = implicitly[ValueReader[T]]
  }

  trait Simple[T] extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
    implicit def arbT: Arbitrary[T]
    implicit def reader: ValueReader[T]
    implicit def writer: ValueWriter[T]

    def validate[F: ValueReader: ValueWriter](value: F)(implicit reader: ValueReader[F], writer: ValueWriter[F]) =
      reader.read(writer.write(value).get) should be(Some(value))

    it("should correctly serialize and parse legal values") {
      forAll { value: T => validate(value) }
    }
  }

  trait WithErrors[T] extends Simple[T] {
    def illegalT: Gen[String]

    it("should refuse illegal values") {
      forAll(illegalT) { str => reader.read(str) should be(None) }
    }
  }
}

class DoubleFormatSpec extends ValueFormatSpec.FromImplicits[Double](Gen.identifier)
class FloatFormatSpec extends ValueFormatSpec.FromImplicits[Float](Gen.identifier)
class LongFormatSpec extends ValueFormatSpec.FromImplicits[Long](Gen.identifier)
class IntFormatSpec extends ValueFormatSpec.FromImplicits[Int](Gen.identifier)
class ShortFormatSpec extends ValueFormatSpec.FromImplicits[Short](Gen.identifier)
class ByteFormatSpec extends ValueFormatSpec.FromImplicits[Byte](Gen.identifier)
class BooleanFormatSpec extends ValueFormatSpec.FromImplicits[Boolean](Gen.identifier)
class LanguageFormatSpec extends ValueFormatSpec.FromImplicits[Language](illegalLanguage)
class LanguagesFormatSpec extends ValueFormatSpec.FromImplicits[Seq[Language]](illegalLanguage)
class CharsetFormatSpec extends ValueFormatSpec.FromImplicits[Charset](illegalCharset)
class EncodingFormatSpec extends ValueFormatSpec.FromImplicits[Encoding](illegalEncoding)
class EncodingsFormatSpec extends ValueFormatSpec.FromImplicits[Seq[Encoding]](illegalEncoding)
class MethodFormatSpec extends ValueFormatSpec.FromImplicits[Method](illegalMethod)
class MethodsFormatSpec extends ValueFormatSpec.FromImplicits[Seq[Method]](illegalMethod)
class MediaTypeFormatSpec extends ValueFormatSpec.FromImplicits[MediaType](illegalMediaType)
class MediaTypesFormatSpec extends ValueFormatSpec.FromImplicits[Seq[MediaType]](illegalMediaType)

class HttpDateFormatSpec extends ValueFormatSpec.Simple[Date] {
  override val arbT   = implicitly[Arbitrary[Date]]
  override val writer = HttpDate
  override val reader = HttpDate
}

class StringFormatSpec extends ValueFormatSpec.Simple[String] {
  override val arbT   = implicitly[Arbitrary[String]]
  override val writer = implicitly[ValueWriter[String]]
  override val reader = implicitly[ValueReader[String]]
}
