package com.nrinaudo.fetch

import scala.util.{Success, Try}
import java.net.URLEncoder


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



  // - Query string parsing --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def apply(query: String): QueryString =
    // Protection against java.net.URL.getQuery returning null rather than the empty string.
    if(query == null) new QueryString()

    else new QueryString(query.split("&").foldLeft(Map[String, List[String]]()) {
      case (map, entry) =>
        val (name, v) = entry.splitAt(entry.indexOf('='))
        val value = v.substring(1)

        // TODO: appending at the end of the list has a complexity of O(n) and is inefficient for long lists. Fix.
        map + (name -> map.get(name).fold(List(value))(_ :+ value))
    })
}

class QueryString(val values: Map[String, List[String]] = Map()) {
  def &[T: ValueWriter](param: (String, T)): QueryString = set(param._1, param._2)


  def add[T: ValueWriter](name: String, values: T*): QueryString =
    ValueWriter.sequence(values).fold(this) { list =>
      new QueryString(this.values + (name -> this.values.get(name).fold(list.toList) {_ ::: list.toList}))
    }


  def set[T: ValueWriter](name: String, values: T*): QueryString = ValueWriter.sequence(values).fold(this) { list =>
    new QueryString(this.values + (name -> list.toList))
  }

  def get[T: ValueReader](name: String): Option[Try[List[T]]] = values.get(name).map { list =>
    ValueReader.sequence(list)(implicitly[ValueReader[T]])
  }

  // TODO: implement proper percent encoding.
  private def encode(str: String) = URLEncoder.encode(str, "utf-8").replaceAll("\\+", "%20")
    .replaceAll("\\%21", "!")
    .replaceAll("\\%27", "'")
    .replaceAll("\\%28", "(")
    .replaceAll("\\%29", ")")
    .replaceAll("\\%7E", "~")

  def writeTo(builder: StringBuilder): StringBuilder = {
    if(values.isEmpty) builder
    else {
      builder.append('?')

      var first = true

      for((name, list) <- values; value <- list) {
        if(first) first = false
        else      builder.append("&")
        builder.append(name).append('=').append(encode(value))
      }

      builder
    }
  }

  override def toString: String = writeTo(new StringBuilder).result()
}