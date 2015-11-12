package com.generativists.thirdway.core

import org.apache.commons.math3.random._
import org.scalatest.{Matchers, FunSpec}

import Implicits._

class RandomImplicitsSpec extends FunSpec with Matchers {
  describe("A java.util.Random") {
    val sourceRng = new java.util.Random

    it("can be implicitly converted to a scala.util.Random"){
      // See: scala/util/Random.scala in the Random companion object
      val desiredRng: scala.util.Random = sourceRng
    }
  }

  describe("A scala.util.Random") {
    val sourceRng = new scala.util.Random

    it("can be implicitly converted to a java.util.Random"){
      // See: scala/util/Random.scala in the Random companion object
      val desiredRng: java.util.Random = sourceRng
    }
  }

  describe("An org.apache.commons.math3.random.RandomGenerator") {
    val sourceRng = new RandomAdaptor(new MersenneTwister())

    describe("wrapped with a RandomAdapter") {
      it("can be implicitly converted to a java.util.Random") {
        val desiredRng: java.util.Random = sourceRng
      }

      it("can be implicitly converted to a scala.util.Random") {
        val desiredRng: scala.util.Random = sourceRng
      }

      describe("and wrapped by a scala.util.Random") {
        it("can be implicitly unwrapped to a RandomGenerator") {
          implicit def random2RandomGenerator(
              r: scala.util.Random
          ): RandomGenerator = {
            r.self.asInstanceOf[RandomGenerator]
          }

          val wrapped = new scala.util.Random(sourceRng)
          val desiredRng: RandomGenerator = wrapped
        }
      }
    }
  }
}
