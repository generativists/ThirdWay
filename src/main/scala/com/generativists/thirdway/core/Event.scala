package com.generativists.thirdway.core

/** Defines an activity to run at a specific, scheduled time and group. */
case class Event[Env] (
  val time: Double,
  val group: Int,
  val activity: Activity[Env]
) extends Ordered[Event[Env]] with Serializable {

  /** Defines the order over (time, group).
    *
    * @note In this method, a time of 5 is less than a time of 10, which is
    *       unsurprising. However, this class exists for use in a priority
    *       queue. In the priority queue, a time of 5 should have a higher
    *       priority than a time of 10. When instantiating the queue, reverse
    *       the order.
    */
  def compare(that: Event[Env]): Int = if (this.time == that.time) {
    this.group compare that.group
  } else {
    this.time compare that.time
  }
}

