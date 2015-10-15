package fetch

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class ValueWriterSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  describe("ValueWriter") {
    it("should sequence empty lists properly") {
      forAll { values: List[Int] =>
        ValueWriter.sequence(values) should be(Some(values.map(_.toString)))
      }
    }

    // TODO: test sequence for objects that fail to serialize
  }
}