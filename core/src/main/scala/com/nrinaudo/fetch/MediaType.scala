package com.nrinaudo.fetch

import java.nio.charset.Charset
import MediaTypeParameters._

object MediaType {
  case class Specific(main: String, sub: String, params: MediaTypeParameters = new MediaTypeParameters()) extends MediaType {
    override def rawType: String = "%s/%s" format (main, sub)
    override def params(values: MediaTypeParameters): MediaType = copy(params = values)
  }

  case class Range(main: String, params: MediaTypeParameters = new MediaTypeParameters()) extends MediaType {
    override def rawType: String = "%s/*" format main
    override def params(values: MediaTypeParameters): MediaType = copy(params = values)

    def sub(value: String): MediaType = Specific(main, value, params)
    def /(value: String): MediaType = sub(value)
  }

  case class All(params: MediaTypeParameters = new MediaTypeParameters()) extends MediaType {
    override def rawType: String = "*/*"
    override def params(values: MediaTypeParameters): MediaType = copy(params = values)
  }

  trait Grammar extends HttpGrammar {
    def all: Parser[MediaType]       = "*" ~ "/" ~ "*" ^^ { case _ => Everything }
    def range: Parser[MediaType]     = token <~ ("/" ~ "*") ^^ { case main => Range(main) }
    def full: Parser[MediaType]      = (token <~ "/") ~ token ^^ { case (main ~ sub) => Specific(main, sub) }
    def mediaType: Parser[MediaType] = (all | range | full) ~ opt(paramSep ~> parameters) ^^ {
      case media ~ None         => media
      case media ~ Some(params) => media.params(new MediaTypeParameters(params))
    }
  }

  private object Format extends Grammar {
    def apply(string: String): Option[MediaType] = parseAll(mediaType, string).map(Some(_)).getOrElse(None)
  }

  def parse(str: String): Option[MediaType] = Format(str)

  def unapply[T](res: Response[T]): Option[MediaType] = res.contentType

  // TODO: complete list on http://en.wikipedia.org/wiki/MIME
  val Everything  = All()
  val Application = Range("application")
  val Text        = Range("text")
  val Image       = Range("image")

  val TextPlain   = Text / "plain"

  val Json        = Application / "json"
  val Xml         = Application / "xml"
  val AtomXml     = Application / "atom+xml"
  val OctetStream = Application / "octet-stream"
}

sealed trait MediaType {
  val params: MediaTypeParameters
  def params(values: MediaTypeParameters): MediaType
  def rawType: String

  override lazy val toString =
    if(params.values.isEmpty) rawType
    else                      rawType + ";" + params.toString

  def unapply[T](res: Response[T]): Option[MediaType] = res.contentType

  def removeParam(name: String): MediaType = params(params.remove(name))

  def param[T: ValueWriter](name: String, value: T): MediaType = params(params.set(name, value))

  def param[T: ValueReader](name: String): Option[T] = params.getOpt[T](name)

  def charset: Option[Charset] = param[Charset]("charset")

  def charset(charset: Charset): MediaType = param("charset", charset)
}