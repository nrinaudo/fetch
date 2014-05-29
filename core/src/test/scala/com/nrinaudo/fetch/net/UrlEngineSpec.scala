package com.nrinaudo.fetch.net

import com.nrinaudo.fetch.EngineSpec

class UrlEngineSpec extends EngineSpec {
  override val httpEngine = UrlEngine()
  override def name   = "UrlEngine"
}
