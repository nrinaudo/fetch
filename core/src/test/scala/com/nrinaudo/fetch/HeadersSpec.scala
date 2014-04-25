package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen
import Gen._
import java.util.Date

object HeadersSpec {
  def date = for(time <- choose(0, 253402300799000l)) yield new Date((time / 1000l) * 1000)

  def cycle[T](format: HeaderFormat[T], value: T) = format.parse(format.format(value))
}

class HeadersSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import HeadersSpec._
  import ConnegSpec._
  import MimeTypeSpec._
  import EncodingSpec._

  def validate[T](format: HeaderFormat[T], value: T) = cycle(format, value) should be(value)

  describe("The date formatter") {
    it("should correctly serialize and parse dates") {
      forAll(date) { date => validate(Headers.DateFormat, date)}
    }
  }

  describe("The language formatter") {
    it("should correctly serialize and parse languages") {
      forAll(language) { lang => validate(Headers.LanguageFormat, lang)}
    }

    it("should correctly serialize and parse lists of languages") {
      forAll(nonEmptyListOf(language)) { langs => validate(Headers.seqFormat(Headers.LanguageFormat), langs)}
    }
  }

  describe("The charset formatter") {
    it("should correctly serialize and parse charsets") {
      forAll(charset) { charset => validate(Headers.CharsetFormat, charset)}
    }

    it("should correctly serialize and parse lists of charsets") {
      forAll(nonEmptyListOf(charset)) { charsets => validate(Headers.seqFormat(Headers.CharsetFormat), charsets)}
    }
  }

  describe("The MIME type formatter") {
    it("should correctly serialize and parse MIME types") {
      forAll(mimeType) { mime => validate(Headers.MimeTypeFormat, mime)}
    }

    it("should correctly serialize and parse lists of MIME types") {
      forAll(nonEmptyListOf(mimeType)) { mimes => validate(Headers.seqFormat(Headers.MimeTypeFormat), mimes)}
    }
  }

  describe("The content encoding formatter") {
    it("should correctly serialize and parse encoings") {
      forAll(encoding) { encoding => validate(Headers.EncodingFormat, encoding)}
    }

    it("should correctly serialize and parse lists of encodings") {
      forAll(nonEmptyListOf(encoding)) { encodings => validate(Headers.seqFormat(Headers.EncodingFormat), encodings)}
    }
  }
}
