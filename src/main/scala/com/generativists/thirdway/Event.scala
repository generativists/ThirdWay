package com.generativists.thirdway

/** Defines an activity to run at a specific, scheduled time. */
case class Event[Env] (
  val time: Double,
  val order: Int,
  val activity: Activity[Env]
) extends Ordered[Event[Env]] with Serializable {

  /** Defines the order over (time, order).
    *
    * @note In this method, a time of 5 is less than a time of 10, which is
    *       unsurprising. However, this class exists for use in a priority
    *       queue. In the priority queue, a time of 5 should have a higher
    *       priority than a time of 10. When instantiating the queue, reverse
    *       the order.
    */
  def compare(that: Event[Env]): Int = if (this.time == that.time) {
    this.order compare that.order
  } else {
    this.time compare that.time
  }
}

