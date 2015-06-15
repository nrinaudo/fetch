package com.nrinaudo.fetch

import java.nio.charset.Charset
import java.util.Date

import com.nrinaudo.fetch.Generators._
import com.nrinaudo.fetch.Headers._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}

object HeaderFormatSpec {
  def cycle[T](value: T)(implicit reader: ValueReader[T], writer: ValueWriter[T]) = reader.read(writer.write(value).get)

  def illegalList[T: Arbitrary: ValueWriter](illegal: Gen[String]): Gen[String] = for {
    l <- listOf(arbitrary[T])
    pos   <- choose(0, l.length - 1)
    (head, tail) = l.map(t => implicitly[ValueWriter[T]].write(t).get).splitAt(pos)
    error <- illegal
  } yield implicitly[ValueWriter[Seq[String]]].write(head ::: (error :: tail)).get

  class FromImplicits[T: ValueReader: ValueWriter: Arbitrary](illegal: Gen[String]) extends ValueFormatSpec.FromImplicits[T](illegal) {
    it("should correctly serialize and parse lists of legal values") {
      forAll(nonEmptyListOf(arbitrary[T])) { ts => validate(ts: Seq[T]) }
    }

    it("should refuse lists containing at least one illegal value") {
      forAll(illegalList[T](illegal)) { ts => implicitly[ValueReader[Seq[T]]].read(ts) should be(None) }
    }
  }
}

// Note that DateFormatSpec doesn't test for lists - the HTTP serialization format doesn't allow for lists of dates.
class DateFormatSpec extends ValueFormatSpec.FromImplicits[Date](illegalDate)
class LanguageFormatSpec extends HeaderFormatSpec.FromImplicits[Language](illegalLanguage)
class CharsetFormatSpec extends HeaderFormatSpec.FromImplicits[Charset](illegalCharset)
class EncodingFormatSpec extends HeaderFormatSpec.FromImplicits[Encoding](illegalEncoding)
class MethodFormatSpec extends HeaderFormatSpec.FromImplicits[Method](illegalMethod)

// TODO: parsing of lists of media types is currently broken and needs fixing.
class MediaTypeFormatSpec extends ValueFormatSpec.FromImplicits[MediaType](illegalMediaType)