package com.generativists.thirdway

import org.scalatest._

class EventSpec extends FlatSpec with Matchers {
  "An Event" should "be ordered by time then order" in {
    val activity = new NoOp[String]

    Event(0.0, 0, activity) should be < Event(1.0, 0, activity)
    Event(0.0, 2, activity) should be > Event(0.0, 1, activity)
    Event(5.0, 5, activity) should be > Event(3.0, 1, activity)
  }
}
