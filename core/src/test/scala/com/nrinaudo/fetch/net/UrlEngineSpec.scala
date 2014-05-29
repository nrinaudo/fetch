package com.nrinaudo.fetch.net

import com.nrinaudo.fetch.EngineSpec

class UrlEngineSpec extends EngineSpec {
  override val engine = UrlEngine()
  override def name   = "UrlEngine"
}
