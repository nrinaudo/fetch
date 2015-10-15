package fetch.net

import fetch.EngineSpec

class UrlEngineSpec extends EngineSpec {
  override val httpEngine = UrlEngine()
  override def name   = "UrlEngine"
}