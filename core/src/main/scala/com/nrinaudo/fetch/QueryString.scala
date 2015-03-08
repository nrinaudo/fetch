package com.nrinaudo.fetch

import scala.util.{Success, Failure, Try}


object QueryString {
  // - Implicit formats ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val Doubles: ValueFormat[Double]   = ValueFormat.Doubles
  implicit val Longs: ValueFormat[Long]       = ValueFormat.Longs
  implicit val Shorts: ValueFormat[Short]     = ValueFormat.Shorts
  implicit val Ints: ValueFormat[Int]         = ValueFormat.Ints
  implicit val Bytes: ValueFormat[Byte]       = ValueFormat.Bytes
  implicit val Floats: ValueFormat[Float]     = ValueFormat.Floats
  implicit val Booleans: ValueFormat[Boolean] = ValueFormat.Booleans
  implicit val Strings: ValueFormat[String]   = ValueFormat.Strings
  implicit val Chars: ValueFormat[Char]       = ValueFormat.Chars



  // - Instance creation -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def apply(content: Map[String, List[String]] = Map()): QueryString = new QueryString(content)

  def apply(query: String): QueryString = {
    def extract(str: String) = new QueryString(str.split("&").foldLeft(Map[String, List[String]]()) {
      case (map, entry) =>
        val (n, v) = entry.splitAt(entry.indexOf('='))

        val name  = UrlEncoder.decode(n)
        val value = UrlEncoder.decode(v.substring(1))

        // TODO: appending at the end of the list has a complexity of O(n) and is inefficient for long lists. Fix.
        map + (name -> map.get(name).fold(List(value))(_ :+ value))
    })

    // Protection against java.net.URL.getQuery returning null rather than the empty string.
    if(query == null || query.isEmpty) new QueryString(Map())
    else if(query.charAt(0) == '?')    extract(query.substring(1))
    else                               extract(query)
  }
}

/** Represents an [[Url]]'s query string. */
class QueryString private (content: Map[String, List[String]]) {
  val values = content.mapValues(_.filter(!_.isEmpty)).filter {
    case (name, entries) => !(name.isEmpty || entries.isEmpty)
  }


  // - Parameter setting -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Returns `Some[List[String]]` if the specified values map to actual content, `None` otherwise.
    *
    * The purpose of this method is to filter out empty value lists.
    */
  private def writeValues[T: ValueWriter](values: T*) =
    ValueWriter.sequence(values).map(_.filter(!_.isEmpty)).filter(!_.isEmpty)

  /** Sets the specified parameter to the specified value.
    *
    * The main purpose of this method is to let application developers write code such as:
    * {{{
    * new QueryString() & ("param1" -> "value1") & ("param2" -> "value2")
    * }}}
    * Note that this method only accepts a single value, and will always replace any previous value for the specified
    * name. If you need to specify the same parameter multiple times, use [[add]] instead.
    *
    * @param  param tuple whose first entry if the parameter's name and second entry its value.
    * @tparam T     type of the value to set.
    * @return       an updated query string.
    */
  def &[T: ValueWriter](param: (String, T)): QueryString = set(param._1, param._2)

  def add[T: ValueWriter](name: String, values: T*): QueryString = writeValues(values: _*).fold(this) { list =>
    new QueryString(this.values + (name -> this.values.get(name).fold(list.toList) {_ ::: list.toList}))
  }

  def set[T: ValueWriter](name: String, values: T*): QueryString = writeValues(values: _*).fold(this) { list =>
    new QueryString(this.values + (name -> list.toList))
  }



  // - Parameter getting -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def getFirst[T: ValueReader](name: String): Option[Try[T]] =  get[T](name) map (_ map (_(0)))

  def get[T: ValueReader](name: String): Option[Try[List[T]]] = values.get(name).map { list =>
    ValueReader.sequence(list) match {
      case Some(l) => Success(l)
      case None    => Failure(new IllegalArgumentException("Illegal value: " + list))
    }
  }

  def getFirstOpt[T: ValueReader](name: String): Option[T] = getOpt[T](name) map(_(0))

  def getOpt[T: ValueReader](name: String): Option[List[T]] = for {
    v1 <- values.get(name)
    v2 <- ValueReader.sequence(v1)(implicitly[ValueReader[T]])
  } yield v2

  def apply[T: ValueReader](name: String): List[T] = values(name) map { v =>
    implicitly[ValueReader[T]].read(v).getOrElse(throw new IllegalArgumentException("Illegal value: " + v))
  }

  def first[T: ValueReader](name: String): T = apply[T](name).apply(0)



  // - Object implementation -------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  override def hashCode(): Int = values.hashCode()

  override def equals(p1: scala.Any): Boolean = p1 match {
    case q: QueryString => q.values == values
    case _              => false
  }



  // - Serialization ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def writeTo(builder: StringBuilder): StringBuilder = {
    var first = true

    for {
      (name, list) <- values
      value        <- list
    } {
      if(first) first = false
      else      builder.append("&")

      builder.append(UrlEncoder.encode(name)).append('=').append(UrlEncoder.encode(value))
    }

    builder
  }

  override lazy val toString: String = writeTo(new StringBuilder).result()
}