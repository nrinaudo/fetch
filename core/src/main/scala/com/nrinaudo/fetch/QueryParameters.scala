package com.nrinaudo.fetch

import KeyValueStore.StringFormats._
import java.net.URLEncoder

object QueryParameters {
  type QueryParameterReader[T] = ValueReader[Seq[String], Seq[T]]
  type QueryParameterWriter[T] = ValueWriter[Seq[T], Seq[String]]
  type QueryParameterFormat[T] = ValueFormat[Seq[String], Seq[T]]

  def apply(value: String): QueryParameters =
    if(value == null) new QueryParameters()
    else new QueryParameters(value.split("&").toList.foldLeft(Map[String, List[String]]()) {
      case (map, entry) =>
        val(name, values) = entry.splitAt(entry.indexOf('='))
        map + (name -> values.substring(1).split(',').toList)
    })

  implicit def toQueryReader[T](implicit reader: ValueReader[String, T]): QueryParameterReader[T] =
    ValueReader.seq(reader)

  implicit def toQueryWriter[T](implicit writer: ValueWriter[T, String]): QueryParameterWriter[T] =
    ValueWriter.seq(writer)

  implicit val IntFormat    = ValueFormat(toQueryReader(Ints), toQueryWriter(Ints))
  implicit val StringFormat = ValueFormat(toQueryReader(Strings), toQueryWriter(Strings))
}

class QueryParameters(override val values: Map[String, List[String]] = Map())
  extends KeyValueStore[List[String], QueryParameters] {
  override def build(values: Map[String, List[String]]): QueryParameters = new QueryParameters(values)

  private def encode(str: String) = URLEncoder.encode(str, "utf-8").replaceAll("\\+", "%20")
    .replaceAll("\\%21", "!")
    .replaceAll("\\%27", "'")
    .replaceAll("\\%28", "(")
    .replaceAll("\\%29", ")")
    .replaceAll("\\%7E", "~")

  override def toString: String = {
    val builder = new StringBuilder()
    var first   = true
    values.foreach {
      case (name, value) =>
        if(first) first = false
        else      builder.append("&")

        builder.append(name).append('=').append(value.map(encode).mkString(","))
    }

    builder.result()
  }
}