package com.nrinaudo.fetch

object Protocol {
  val Http = Protocol("http", Some(80))
  val Https = Protocol("https", Some(443))
}

case class Protocol(value: String, defaultPort: Option[Int])