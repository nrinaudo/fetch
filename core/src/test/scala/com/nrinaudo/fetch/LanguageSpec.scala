package com.nrinaudo.fetch

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import java.util.Locale
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object LanguageSpec {
  implicit val locale: Arbitrary[Locale] = Arbitrary(Gen.oneOf(Locale.ENGLISH, Locale.FRENCH, Locale.GERMAN, Locale.ITALIAN, Locale.JAPANESE,
    Locale.KOREAN, Locale.CHINESE, Locale.SIMPLIFIED_CHINESE, Locale.TRADITIONAL_CHINESE, Locale.FRANCE, Locale.GERMANY,
    Locale.ITALY, Locale.JAPAN, Locale.KOREA, Locale.CHINA, Locale.PRC, Locale.TAIWAN, Locale.UK, Locale.US,
    Locale.CANADA, Locale.CANADA_FRENCH))

  implicit val language: Arbitrary[Language] = Arbitrary(arbitrary[Locale].map(Language.apply))

  def illegalLanguage: Gen[String] = Arbitrary.arbitrary[String].suchThat {_.matches(".*[^a-zA-Z_-].*")}
}

class LanguageSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import LanguageSpec._

  describe("Language") {
    it("should parse valid languages") {
      forAll { lang: Language => Language.parse(lang.toString) should be(Some(lang))}
    }

    it("should apply on valid locales") {
      forAll { locale: Locale => Language(locale).toLocale should be(locale)}
    }

    it("should not parse invalid languages") {
      forAll(illegalLanguage) { lang => Language.parse(lang) should be(None)}
    }
  }
}
