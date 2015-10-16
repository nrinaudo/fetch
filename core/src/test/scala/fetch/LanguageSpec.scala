package fetch

import fetch.grammar.language
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import fetch.Generators._

class LanguageSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("Language") {
    it("should parse valid languages") {
      forAll { lang: Language => Language.parse(grammar.language(lang.main, lang.sub)) should be(Some(lang)) }
    }

    it("should not parse invalid languages") {
      forAll(illegalLanguage) { lang => Language.parse(lang) should be(None)}
    }
  }
}