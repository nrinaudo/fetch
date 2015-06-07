package com.nrinaudo.fetch

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}

/** Defines all the common generators used by our property tests. */
object Generators {
  // - Parameter generators --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Param(name: String, value: String)

  implicit val arbParam: Arbitrary[Param] = Arbitrary {
    for {
      name <-  identifier
      value <- nonEmptyListOf(oneOf(32.toChar to 126.toChar)).map(_.mkString)
    } yield Param(name, value)
  }

  // Note: we don't generate more than 5 parameters because this causes unrelated issues when testing - some HTTP
  // servers seem to have a limit to the size of a header value.
  implicit val arbParams: Arbitrary[Parameters] = Arbitrary {
    for {
      size <- choose(0, 5)
      ps   <- mapOfN(size, arbitrary[Param].map(p => p.name -> p.value))
    } yield Parameters(ps)
  }

  def illegalParams: Gen[String] = Arbitrary.arbitrary[String].suchThat(str => !str.contains('=') && !str.isEmpty)



  // - MediaType generators --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def mainType: Gen[String] = Gen.oneOf("text", "application", "video", "audio", "image", "message", "multipart")
  def subType: Gen[String] = Gen.oneOf("plain", "png", "jpg", "rdf", "html", "rdf+xml", "json", "x-fixed-field")

  implicit val arbMediaAll: Arbitrary[MediaType.All] = Arbitrary(Gen.const(MediaType.Everything))
  implicit val arbMediaRange: Arbitrary[MediaType.Range] = Arbitrary(for(main <- mainType) yield MediaType.Range(main))
  implicit val arbMediaSpecific: Arbitrary[MediaType.Specific] = Arbitrary {
    for {
      main   <- mainType
      sub    <- subType
    } yield MediaType.Specific(main, sub)
  }

  implicit val arbMediaType: Arbitrary[MediaType] = Arbitrary {
    for {
      mediaType <- Gen.oneOf(arbitrary[MediaType.All], arbitrary[MediaType.Range], arbitrary[MediaType.Specific])
      // Note: q is not considered a valid media type parameter, as it would conflict with the Accept header's q parameter.
      params    <- arbitrary[Parameters].map(p => p.remove("q"))
    } yield mediaType.params(params)
  }

  def illegalMediaType: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.indexOf('/') == -1)


  // - Protocol generators ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Generates a random supported protocol (http or https). */
  implicit val arbProtocol: Arbitrary[Protocol] = Arbitrary(Gen.oneOf(Protocol.Http, Protocol.Https))

  /** Generates invalid protocol names. */
  def invalidProtocol: Gen[String] = Arbitrary.arbitrary[String].suchThat(s => s != Protocol.Http.name && s != Protocol.Https.name)


  // - QueryString generators ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Returns a single query parameter. */
  def queryParam: Gen[(String, List[String])] = for {
    name   <- arbitrary[String].suchThat(!_.isEmpty)
    count  <- choose(1, 5)
    values <- listOfN(count, arbitrary[String].suchThat(!_.isEmpty))
  } yield (name, values)

  def queryParams: Gen[Map[String, List[String]]] = for {
    count  <- choose(1, 10)
    params <- listOfN(count, queryParam)
  } yield params.foldLeft(Map(): Map[String, List[String]]) { (map, param) => map + param}

  /** Returns a query string. */
  implicit val arbQueryString: Arbitrary[QueryString] = Arbitrary {
    for {
      query <- queryParams
    } yield QueryString(query)
  }


  // - URL generators --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def domainSeg: Gen[String] = for {
    first   <- alphaChar
    length  <- choose(1, 10)
    content <- listOfN(length, alphaLowerChar)
  } yield (first :: content).mkString

  /** Generates a valid host. */
  def host: Gen[String] = for {
    name <- domainSeg
    ext  <- oneOf("com", "fr", "es", "it", "co.uk", "co.jp", "io")
  } yield name + "." + ext

  /** Generates a valid port. */
  def port: Gen[Int] = choose(1, 65535)

  def segment: Gen[String] = arbitrary[String].suchThat(!_.isEmpty)

  /** Generates a valid path. */
  def path: Gen[List[String]] = for {
    count <- choose(0, 5)
    path <- listOfN(count, segment)
  } yield path

  def fragment: Gen[Option[String]] = oneOf(true, false) flatMap {b =>
    if(b) None
    else  arbitrary[String].map(Some(_))
  }

  implicit val arbUrl: Arbitrary[Url] = Arbitrary {
    for {
      pr <- arbitrary[Protocol]
      h  <- host
      p  <- port
      s  <- path
      q  <- arbitrary[QueryString]
      r  <- fragment
    } yield Url(pr, h, p, s, q, r)
  }
}
