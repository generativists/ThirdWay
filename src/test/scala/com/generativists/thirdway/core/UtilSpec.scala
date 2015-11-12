package com.generativists.thirdway.core

import scala.collection.mutable
import org.apache.commons.math3.random.MersenneTwister
import org.scalatest.{FunSpec, Matchers}


class UtilSpec extends FunSpec with Matchers {
  describe("serializingClone") {
    it("should produce a copy unconnected to the original") {
      val original = mutable.Map(
        1 -> mutable.ListBuffer(1, 2, 3)
      )
      val copied = Util.serializingClone(original)
      copied shouldEqual original

      copied(1)(1) = 100 // Mutate an element in the copied Map
      copied shouldNot equal(original)

      copied(1)(1) = original(1)(1) // Reset the element to its original value
      copied shouldEqual original
    }
  }

  describe("shuffleInPlace") {
    it("should randomize the order of a given collection") {
      val rng = new MersenneTwister()
      val symbols = List('A, 'B, 'C)
      val counts = symbols.map { sym => sym -> Array(0, 0, 0) }.toMap

      val n = 1000
      (0 until n) foreach { _ =>
        val items = mutable.ArrayBuffer(symbols: _*)
        Util.shuffleInPlace(rng, items)
        items.zipWithIndex.foreach { case (sym, i) => counts(sym)(i) += 1 }
      }

      for ((sym, tallies) <- counts; tally <- tallies) {
        // Expectation is in the 99.998% CI ~ Binomial(1000, 1/3)
        tally should be(333 +- 65)
      }
    }
  }
}
