package com.generativists.thirdway.core

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
}
