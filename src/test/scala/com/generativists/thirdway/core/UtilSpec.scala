package com.generativists.thirdway.core

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class UtilSpec extends FlatSpec with Matchers {
  "serializingClone" should "produce a copy unconnected to the original" in {
    val original = mutable.Map(
      1 -> mutable.ListBuffer(1, 2, 3)
    )
    val copied = Util.serializingClone(original)

    copied shouldEqual original

    copied(1)(1) = 100
    copied shouldNot equal(original)

    copied(1)(1) = 2
    copied shouldEqual original
  }

  "shuffleInPlace" should "randomize the order of a given collection" in {
    val rng = new MersenneTwister()
    val symbols = List('A, 'B, 'C)
    val counts = symbols.map { sym => sym -> Array(0, 0, 0) }.toMap

    val n = 1000
    (0 until n) foreach { _ =>
      val items = mutable.ArrayBuffer(symbols: _*)
      Util.shuffleInPlace(rng, items)
      items.zipWithIndex.foreach { case (sym, i) => counts(sym)(i) += 1 }
    }

    val (lowerBound, upperBound) = (271.0, 398.0)  // 99.998 % CI

    for((sym, tallies) <- counts; tally <- tallies) {
      (tally > lowerBound) shouldEqual true  // Weird ScalaTest issues
      (tally < upperBound) shouldEqual true
    }
  }
}
