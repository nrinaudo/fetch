package com.nrinaudo.fetch

import org.scalacheck.Gen._
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import com.nrinaudo.fetch.HeaderFormat._
import scala.util.Success

object HeaderFormatSpec {
  def cycle[T](format: HeaderFormat[T], value: T) = format.read(format.write(value).get)
}

class HeaderFormatSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import HeaderSpec._
  import HeaderFormatSpec._
  import ConnegSpec._
  import MimeTypeSpec._
  import EncodingSpec._
  import ByteRangeSpec._

  def validate[T](format: HeaderFormat[T], value: T) = cycle(format, value) should be(Success(value))

  describe("The date formatter") {
    it("should correctly serialize and parse dates") {
      forAll(date) { date => validate(DateFormat, date)}
    }
  }

  describe("The language formatter") {
    it("should correctly serialize and parse languages") {
      forAll(language) { lang => validate(LanguageFormat, lang)}
    }

    it("should correctly serialize and parse lists of languages") {
      forAll(nonEmptyListOf(language)) { langs => validate(compositeFormat(LanguageFormat), langs)}
    }
  }

  describe("The charset formatter") {
    it("should correctly serialize and parse charsets") {
      forAll(charset) { charset => validate(CharsetFormat, charset)}
    }

    it("should correctly serialize and parse lists of charsets") {
      forAll(nonEmptyListOf(charset)) { charsets => validate(compositeFormat(CharsetFormat), charsets)}
    }
  }

  describe("The MIME type formatter") {
    it("should correctly serialize and parse MIME types") {
      forAll(mimeType) { mime => validate(MimeTypeFormat, mime)}
    }

    it("should correctly serialize and parse lists of MIME types") {
      forAll(nonEmptyListOf(mimeType)) { mimes => validate(compositeFormat(MimeTypeFormat), mimes)}
    }
  }

  describe("The content encoding formatter") {
    it("should correctly serialize and parse encodings") {
      forAll(encoding) { encoding => validate(EncodingFormat, encoding)}
    }

    it("should correctly serialize and parse lists of encodings") {
      forAll(nonEmptyListOf(encoding)) { encodings => validate(compositeFormat(EncodingFormat), encodings)}
    }
  }

  describe("The byte range encoding formatter") {
    it("should correctly serialize and parse byte ranges") {
      forAll(byteRange) { range => validate(ByteRangeFormat, range)}
    }

    it("should correctly serialize and parse lists of byte ranges") {
      forAll(nonEmptyListOf(byteRange)) { ranges => validate(ByteRangesFormat, ranges)}
    }
  }
}