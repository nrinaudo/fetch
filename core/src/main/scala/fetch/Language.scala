package fetch

import fastparse.all._

/** Tools for creating instances of [[Language]]. */
object Language {
  private[fetch] val parser: Parser[Language] = grammar.language.map {
    case (main, sub) => Language(main, sub.toList)
  }

  def parse(str: String): Option[Language] = parseFully(parser, str)

  def apply(main: String): Language = Language(main, List.empty)
}

/** Represents an entity's language.
  *
  * These can be used for content negotiation or
  * to describe the language in which a response is written.
  */
case class Language(main: String, sub: List[String])