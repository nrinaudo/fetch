package com.nrinaudo.fetch

import java.nio.charset.Charset
import MediaTypeParameters._

/** Defines [[MediaType]] implementations as well as known types. */
object MediaType {
  // - MediaType implementations ---------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Specific media type, with both a main- and sub-type (such as `text-plain`, for example). */
  final case class Specific(main: String, sub: String, params: MediaTypeParameters = new MediaTypeParameters())
    extends MediaType {
    override def rawType: String = "%s/%s" format (main, sub)
    override def params(values: MediaTypeParameters): MediaType = copy(params = values)

    /** Allows specific media types to be used in pattern matching.
      *
      * Note that this method ignores media type parameters. For example, the following expression will match regardless
      * of media type parameters: {{{
      *  val req: Request[Response[ResponseEntity]] = ???
      *
      *  req.map {
      *    case MediaType.Json(res) => println("JSON: " + res.body.as[String])
      *    case _                   => println("Unsupported media type")
      *  }
      * }}}
      */
    override def unapply[T](res: Response[T]): Option[Response[T]] = res.contentType flatMap {
      case Specific(m, s, _) if main == m && sub == s => Some(res)
      case _                                          => None
    }
  }

  /** Media range, with a main-type only (such as `text/ *`, for example). */
  final case class Range(main: String, params: MediaTypeParameters = new MediaTypeParameters()) extends MediaType {
    override def rawType: String = "%s/*" format main
    override def params(values: MediaTypeParameters): MediaType = copy(params = values)

    /** Allows media ranges to be used in pattern matching.
      *
      * Note that this method ignores media type parameters. For example, the following expression will match regardless
      * of media type parameters: {{{
      *  val req: Request[Response[ResponseEntity]] = ???
      *
      *  req.map {
      *    case MediaType.Text(res) => println("Text: " + res.body.as[String])
      *    case _                   => println("Unsupported media type")
      *  }
      * }}}
      */
    override def unapply[T](res: Response[T]): Option[Response[T]] = res.contentType flatMap {
      case Specific(m, s, _) if m == main => Some(res)
      case Range(m, _) if m == main       => Some(res)
      case _                              => None
    }

    def sub(value: String): MediaType = Specific(main, value, params)
    def /(value: String): MediaType = sub(value)
  }

  /** All media types: `* / *` **/
  final case class All(params: MediaTypeParameters = new MediaTypeParameters()) extends MediaType {
    override def rawType: String = "*/*"
    override def params(values: MediaTypeParameters): MediaType = copy(params = values)

    /** Allows the "any" media type to be used to in pattern matching.
      *
      * Note that this method ignores media type parameters. For example, the following expression will match regardless
      * of media type parameters: {{{
      *  val req: Request[Response[ResponseEntity]] = ???
      *
      *  req.map {
      *    case MediaType.All(res) => println("Has media type: " + res.body.as[String])
      *    case _                  => println("No media type")
      *  }
      * }}}
      *
      * The only way for a response not to be matched is for it not to have a media type at all.
      */
    override def unapply[T](res: Response[T]): Option[Response[T]] = res.contentType.map(c => res)
  }



  // - Parsing ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Defines parses for the media type syntax. */
  trait Grammar extends HttpGrammar {
    /** Parses `* / *`. */
    def all: Parser[MediaType]      = "*" ~ "/" ~ "*" ^^ { case _ => Everything }
    /** Parses `main/ *`. */
    def range: Parser[MediaType]    = token <~ ("/" ~ "*") ^^ { case main => Range(main) }
    /** Parses `main/sub`. */
    def specific: Parser[MediaType] = (token <~ "/") ~ token ^^ { case (main ~ sub) => Specific(main, sub) }

    /** Parses a media type and its parameters. */
    def mediaType: Parser[MediaType] = (all | range | specific) ~ opt(paramSep ~> parameters) ^^ {
      case media ~ None         => media
      case media ~ Some(params) => media.params(new MediaTypeParameters(params))
    }
  }

  private object Format extends Grammar {
    def apply(string: String): Option[MediaType] = parseAll(mediaType, string).map(Some(_)).getOrElse(None)
  }

  /** Attempts to extract a media type from the specified string. */
  def parse(str: String): Option[MediaType] = Format(str)

  /** Allows pattern matching against instances of [[Response]], as in {{{
    *  val req: Request[Response[ResponseEntity]] = ???
    *
    *  req.map {
    *    case MediaType(type) => println("Media type: " + type)
    *  }
    * }}}
    */
  def unapply[T](res: Response[T]): Option[MediaType] = res.contentType



  // - Known types -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val Everything  = All()

  // Application types
  val Application = Range("application")
  val Atom        = Application / "atom+xml"
  val EcmaScript  = Application / "ecmascript"
  val Json        = Application / "json"
  val JavaScript  = Application / "javascript"
  val OctetStream = Application / "octet-stream"
  val Pdf         = Application / "pdf"
  val PostScript  = Application / "postscript"
  val RdfXml      = Application / "rdf+xml"
  val RssXml      = Application / "rss+xml"
  val SoapXml     = Application / "soap+xml"
  val Woff        = Application / "font-woff"
  val Xhtml       = Application / "xhtml+xml"
  val Xml         = Application / "xml"
  val Dtd         = Application / "xml-dtd"
  val Zip         = Application / "zip"
  val Gzip        = Application / "gzip"

  // Image types
  val Image = Range("image")
  val Gif   = Image / "gif"
  val Jpeg  = Image / "jpeg"
  val Png   = Image / "png"
  val Svg   = Image / "svg+xml"

  // Text types
  val Text      = Range("text")
  val Css       = Text / "css"
  val Csv       = Text / "csv"
  val Html      = Text / "html"
  val PlainText = Text / "plain"
  val Rtf       = Text / "rtf"
  val Vcard     = Text / "vcard"
}

sealed trait MediaType {
  /** Parameters associated with this instance. */
  val params: MediaTypeParameters
  /** Raw string representation of the media type, ignoring parameters. */
  def rawType: String
  /** Creates a copy of the current instance with the specified parameters. */
  def params(values: MediaTypeParameters): MediaType

  override lazy val toString =
    if(params.values.isEmpty) rawType
    else                      rawType + ";" + params.toString

  def unapply[T](res: Response[T]): Option[Response[T]]

  /** Removes the specified parameter. */
  def removeParam(name: String): MediaType =
    if(params.contains(name)) params(params.remove(name))
    else                      this

  /** Sets the specified parameter to the specified value. */
  def param[T: ValueWriter](name: String, value: T): MediaType = params(params.set(name, value))

  /** Returns the value of the requested parameter.
    *
    * This is strictly a convenience method and will simply call the underlying parameter's
    * [[MediaTypeParameters.getOpt]] method.
    */
  def param[T: ValueReader](name: String): Option[T] = params.getOpt[T](name)

  /** Sets the charset associated with this media type (`charset` parameter). */
  def charset: Option[Charset] = param[Charset]("charset")

  /** Returns the charset associated with this media type (`charset` parameter). */
  def charset(charset: Charset): MediaType = param("charset", charset)
}