package com.nrinaudo.fetch

import java.util.Locale

object Language {
  trait Grammar extends HttpGrammar {
    def tag: Parser[String] = """\p{Alpha}{1,8}""".r

    def language: Parser[Language] = tag ~ opt("-" ~> tag) ^^ {
      case main ~ Some(sub) => Specific(main, sub)
      case main ~ _         => General(main)
    }
  }

  private object Format extends Grammar {
    def apply(value: String): Option[Language] = parseAll(language, value).map(Some(_)).getOrElse(None)
  }

  def unapply(str: String): Option[Language] = Format(str)

  def apply(locale: Locale): Language =
    if(locale.getCountry.isEmpty) General(locale.getLanguage)
    else                          Specific(locale.getLanguage, locale.getCountry)

  def apply(str: String): Language = unapply(str) getOrElse {
    throw new IllegalArgumentException("Illegal language: " + str)
  }

  final case class General(value: String) extends Language {
    override def toLocale: Locale = new Locale(value)
    override def toString = value
  }

  final case class Specific(main: String, sub: String) extends Language {
    override def toLocale: Locale = new Locale(main, sub)
    override def toString = "%s-%s" format (main, sub)
  }
}

sealed trait Language {
  def toLocale: Locale
}