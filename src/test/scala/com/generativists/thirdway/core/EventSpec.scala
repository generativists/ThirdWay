package com.generativists.thirdway.core

import org.scalatest.{FunSpec, Matchers}


class EventSpec extends FunSpec with Matchers {
  describe("An Event") {
    it("should be ordered by time then order") {
      val activity = new NoOp[String]

      Event(0.0, 0, activity) should be < Event(1.0, 0, activity)
      Event(0.0, 2, activity) should be > Event(0.0, 1, activity)
      Event(5.0, 5, activity) should be > Event(3.0, 1, activity)
    }
  }
}
