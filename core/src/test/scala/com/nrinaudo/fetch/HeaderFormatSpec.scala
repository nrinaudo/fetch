package com.nrinaudo.fetch

import org.scalacheck.Gen._
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import com.nrinaudo.fetch.Headers._
import scala.util.Success
import java.util.Locale
import java.nio.charset.Charset
import org.scalacheck.{Gen, Arbitrary}

object HeaderFormatSpec {
  def cycle[T](format: ValueFormat[T], value: T) = format.read(format.write(value).get)
}

class HeaderFormatSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import HeadersSpec._
  import HeaderFormatSpec._
  import ConnegSpec._
  import MimeTypeSpec._
  import EncodingSpec._
  import ByteRangeSpec._

  def validate[T](format: ValueFormat[T], value: T) = cycle(format, value) should be(Success(value))

  describe("The date formatter") {
    it("should correctly serialize and parse dates") {
      forAll(date) { date => validate(DateFormat, date)}
    }

    it("should refuse illegal dates") {
      // Starting the string with an arbitrary letter ensures it's not a valid date.
      forAll(Gen.alphaChar, Arbitrary.arbitrary[String]) {(c, str) => DateFormat.read(c + str).isFailure should be(true)}
    }
  }

  describe("The language formatter") {
    it("should correctly serialize and parse languages") {
      forAll(language) { lang => validate(LanguageFormat, lang)}
    }

    it("should correctly serialize and parse lists of languages") {
      forAll(nonEmptyListOf(language)) { langs => validate(compositeFormat[Locale], langs)}
    }

    it("should refuse illegal languages") {
      // Starting the language with an arbitrary number ensures it's not a valid language.
      forAll(Gen.numChar, Arbitrary.arbitrary[String]) {(c, str) => LanguageFormat.read(c + str).isFailure should be(true)}
    }
  }

  describe("The charset formatter") {
    it("should correctly serialize and parse charsets") {
      forAll(charset) { charset => validate(CharsetFormat, charset)}
    }

    it("should correctly serialize and parse lists of charsets") {
      forAll(nonEmptyListOf(charset)) { charsets => validate(compositeFormat[Charset], charsets)}
    }

    it("should refuse illegal charsets") {
      forAll(Arbitrary.arbitrary[String].suchThat(!Charset.availableCharsets().containsKey(_))) { str =>
        CharsetFormat.read(str).isFailure should be(true)
      }
    }
  }

  describe("The MIME type formatter") {
    it("should correctly serialize and parse MIME types") {
      forAll(mimeType) { mime => validate(MimeTypeFormat, mime)}
    }

    it("should correctly serialize and parse lists of MIME types") {
      forAll(nonEmptyListOf(mimeType)) { mimes => validate(compositeFormat[MimeType], mimes)}
    }

    it("should refuse illegal MIME types") {
      forAll(Arbitrary.arbitrary[String].suchThat(_.indexOf('/') == -1)) { str =>
        MimeTypeFormat.read(str).isFailure should be(true)
      }
    }
  }

  describe("The content encoding formatter") {
    it("should correctly serialize and parse encodings") {
      forAll(encoding) { encoding => validate(EncodingFormat, encoding)}
    }

    it("should correctly serialize and parse lists of encodings") {
      forAll(nonEmptyListOf(encoding)) { encodings => validate(compositeFormat[Encoding], encodings)}
    }

    // TODO: failure cases.
  }

  describe("The byte range encoding formatter") {
    it("should correctly serialize and parse byte ranges") {
      forAll(byteRange) { range => validate(ByteRangeFormat, range)}
    }

    it("should correctly serialize and parse lists of byte ranges") {
      forAll(nonEmptyListOf(byteRange)) { ranges => validate(ByteRangesFormat, ranges)}
    }

    // TODO: failure cases.
  }

  describe("The method formatter") {
    it("should correctly serialize and parse methods") {
      forAll(MethodSpec.httpMethod) { method => validate(MethodFormat, method)}
    }

    it("should correctly serialize and parse lists of byte ranges") {
      forAll(nonEmptyListOf(MethodSpec.httpMethod)) { methods => validate(compositeFormat[Method], methods)}
    }

    it("should refuse illegal methods") {
      forAll(identifier, identifier) { (a, b) =>
        MethodFormat.read(a + ' ' + b).isFailure should be(true)
      }
    }
  }
}