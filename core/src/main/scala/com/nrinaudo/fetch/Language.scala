package com.nrinaudo.fetch

import java.util.Locale

/** Tools for creating instances of [[Language]]. */
object Language {
  /** Describes the grammar used to read and write languages. */
  trait Grammar extends HttpGrammar {
    def tag: Parser[String] = """\p{Alpha}{1,8}""".r

    def language: Parser[Language] = tag ~ opt("-" ~> tag) ^^ {
      case main ~ Some(sub) => CountryLanguage(main, sub)
      case main ~ _         => GlobalLanguage(main)
    }
  }

  private object Format extends Grammar {
    def apply(value: String): Option[Language] = parseAll(language, value).map(Some(_)).getOrElse(None)
  }

  /** `String`-based extractor. */
  def unapply(str: String): Option[Language] = Format(str)

  /** `Locale`-based extractor. */
  def unapply(locale: Locale): Option[Language] = Some(apply(locale))

  /** Returns the instance of [[Language]] that matches the specified locale. */
  def apply(locale: Locale): Language =
    if(locale.getCountry.isEmpty) GlobalLanguage(locale.getLanguage)
    else                          CountryLanguage(locale.getLanguage, locale.getCountry)

  /** Attempts to extract a valid instance from the specified string. */
  def apply(str: String): Language = unapply(str) getOrElse {
    throw new IllegalArgumentException("Illegal language: " + str)
  }
}

/** Represents a global language, regardless of regional versions.
  *
  * English, for example, is a global language, as opposed to American English which is specific to the US.
  *
  * @param lang code of the language (for example, `en`, `fr`....).
  */
final case class GlobalLanguage(lang: String) extends Language {
  override def toLocale: Locale = new Locale(lang)
  override def toString = lang
}

/** Represents a regional version of a given language.
  *
  * Canadian French, for example, is a region specific version of the global French language.
  *
  * @param lang    code of the language (for example, `en`, `fr`...).
  * @param country code of the country (for example, `FR`, `US`...).
  */
final case class CountryLanguage(lang: String, country: String) extends Language {
  override def toLocale: Locale = new Locale(lang, country)
  override def toString = "%s-%s" format (lang, country)
}

/** Represents an entity's language.
  *
  * These can be used for [[com.nrinaudo.fetch.Request.acceptLanguage content negotiation]] or
  * to describe the language in which a [[com.nrinaudo.fetch.Response.contentLanguage response]] is written.
  *
  * A language can either be [[GlobalLanguage global]] or [[CountryLanguage country-specific]].
  */
sealed trait Language {
  /** Returns the instance of `Locale` associated with this language. */
  def toLocale: Locale
}