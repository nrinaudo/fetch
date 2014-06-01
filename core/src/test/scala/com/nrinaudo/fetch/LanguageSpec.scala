package com.nrinaudo.fetch

import org.scalacheck.{Arbitrary, Gen}
import java.util.Locale
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object LanguageSpec {
  def locale: Gen[Locale] = Gen.oneOf(Locale.ENGLISH, Locale.FRENCH, Locale.GERMAN, Locale.ITALIAN, Locale.JAPANESE,
    Locale.KOREAN, Locale.CHINESE, Locale.SIMPLIFIED_CHINESE, Locale.TRADITIONAL_CHINESE, Locale.FRANCE, Locale.GERMANY,
    Locale.ITALY, Locale.JAPAN, Locale.KOREA, Locale.CHINA, Locale.PRC, Locale.TAIWAN, Locale.UK, Locale.US,
    Locale.CANADA, Locale.CANADA_FRENCH)

  def language: Gen[Language] = locale.map(Language.apply)

  def illegalLanguage = Arbitrary.arbitrary[String].suchThat {_.matches(".*[^a-zA-Z_-].*")}
}

class LanguageSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import LanguageSpec._

  describe("Language") {
    it("should unapply on valid languages") {
      forAll(language) { lang => Language.unapply(lang.toString) should be(Some(lang))}
    }

    it("should unapply on valid locales") {
      forAll(locale) { locale => Language.unapply(locale).map(_.toLocale) should be(Some(locale))}
    }

    it("should apply on valid languages") {
      forAll(language) { lang => Language(lang.toString) should be(lang)}
    }

    it("should apply on valid locales") {
      forAll(locale) { locale => Language(locale).toLocale should be(locale)}
    }

    it("should not unapply on invalid languages") {
      forAll(illegalLanguage) { lang => Language.unapply(lang) should be(None)}
    }

    it("should fail to apply on invalid languages") {
      forAll(illegalLanguage) { lang => intercept[IllegalArgumentException] { Language(lang) }}
    }
  }
}
