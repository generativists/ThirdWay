package com.generativists.thirdway

import scala.collection.generic.Clearable
import scala.collection.mutable
import scala.math.ulp

class Schedule[Env] (
  var time: Double,
  var step: Long,
  val rng: RNG
) extends Clearable with Serializable {

  /** The queue is the core of scheduling. It prioritizes `Event`s in reverse
    * order, so that the item with the lowest time then the lowest ordering
    * goes first.
    */
  protected val queue = mutable.PriorityQueue[Event[Env]]()(
    Ordering[Event[Env]].reverse
  )

  /** The `runOneStep` method collects `Event`s that should run next into a
    * collection. Rather than reallocating that collection each time,
    * `Schedule` allocates it once in `shuffleTmp`.  Then, the `Schedule`
    * clears it as needed.
    */
  protected val shufflerTmp = mutable.ArrayBuffer.empty[Event[Env]]

  /** @return the number of enqueued `Event`s */
  def length  = queue.length

  /** @return true if any `Event`s are enqueued */
  def isEmpty = queue.isEmpty

  /** @return true if the schedule was ever run and it is now empty */
  def isExhausted = step != 0 && isEmpty

  /** Resets all items back to their pristine state, except for the RNG */
  def reset() = {
    time = Schedule.BeforeSimulation
    step = 0
    queue.clear()
  }

  /** Empty the queue but maintain all other state variables. */
  def clear() = queue.clear()

  /** Empty the queue and return all `Event`s in prioritized order. */
  def dequeueAll() = queue.dequeueAll

  /** Add an event to the `Schedule`.
    *
    * To maintain a hygienic schedule, the `at` time must meet several
    * preconditions.
    *
    * - It must be less than the `MaximumTime`.
    * - It must not be `NaN`.
    * - It cannot precede or equal the `Epoch`
    * - It cannot precede the current schedule time.
    *
    * Violation of any of these conditions ​results in a raised ArgumentError.
    *
    * Additionally, `at` is always greater than the `Schedule` time. However,
    * this method does not thrown an exception if `at == this.time`. Instead,
    * `at` is incremented by the smallest amount possible. Effectively,
    * scheduling at the current time asks the scheduler to run something at
    * the next tick.
    *
    * @param at the schedule time at which the event should activate
    * @param order the activation order relative to other events at this time
    * @param activity the activity to run
    */
  def enqueue(at: Double, order: Int = 0, activity: Activity[Env]): Unit = {
    require(!at.isNaN,                 "Scheduled at NaN")  // Must be first
    require(at < Schedule.MaximumTime, "Schedule already at MaximumTime")
    require(at >= Schedule.Epoch,      "Scheduled before Epoch")
    require(at >= time,                "Scheduled in the past")

    if(time == at) {
      queue.enqueue(Event(at + ulp(at), order, activity))
    } else {
      queue.enqueue(Event(at, order, activity))
    }
  }

  /** Schedule an activity to run once at a given time and order. */
  def once(activity: Activity[Env], time: Double, order: Int = 0): Unit = {
    enqueue(time, order, activity)
  }

  /** Schedule an activity to run once at time+delta and order. */
  def onceIn(activity: Activity[Env], delta: Double, order: Int = 0): Unit = {
    enqueue(time + delta, order, activity)
  }

  /** Schedule an action run repeatedly at a fixed interval.
    *
    * @param activity the activity to run repeatedly
    * @param startAt the initial scheduled time
    * @param interval the interval (in time) for rescheduling
    * @param order the order relative to other `Event`s
    *
    * @return a Stoppable for possible early termination
    */
  def repeating(
    activity: Activity[Env],
    startAt: Double=1.0,
    interval: Double=1.0,
    order: Int=0
  ): Stoppable[Env] = {
    require(interval > 0)
    val repeatingActivity = RepeatingActivity(activity, interval, order)
    enqueue(startAt, order, repeatingActivity)
    repeatingActivity
  }

  /** Runs the schedule one step forward over some environment.
    *
    * A step corresponds to a time with scheduled activities. For example,
    * given two activities at 1.0, one at 3.0, and one at 9.0, the
    * corresponding step (after each time runs) is 1, 2, and 3, respectively.
    *
    * Activities scheduled at the same time but with a different order run
    * over the same step. However, activities with a lower order (i.e. higher
    * priority) are guaranteed to run first. Activities at the same time and
    * the same order run in a random order.
    *
    * @param env the environment given to each activity
    *
    * @return true if the any activity was executed, false otherwise
    */
  def runOneStep(env: Env): Boolean = {
    if(time >= Schedule.MaximumTime || queue.isEmpty) {
      return false
    }

    val runTime = queue.head.time
    var currentOrder = queue.head.order

    def shuffleAndRun(): Unit = {
      Util.shuffleInPlace(rng, shufflerTmp)
      shufflerTmp foreach { _.activity(env, this) }
      shufflerTmp.clear()
    }

    // Collect all events at the current time, grouping by the order field.
    // Shuffle and run each group.
    var continue = true
    while(continue) {
      val nextEvent = queue.dequeue()

      if(nextEvent.order != currentOrder) {
        shuffleAndRun()
        currentOrder = nextEvent.order
      }

      shufflerTmp.append(nextEvent)

      continue = !isEmpty && queue.head.time == runTime
    }

    if(!shufflerTmp.isEmpty) { shuffleAndRun() }

    // Increment the time and step
    time = runTime
    step += 1

    return true
  }

  /** Merge two schedules.
    *
    * This method schedules the activities of `other` on the called schedule.
    * However, it **does not** remove them from `other`. Call `that.clear()`
    * afterwards if that is the desired behavior.
    *
    * Raises an `ArgumentError if the the `other` schedule has events in this
    * schedules past.
    */
  def merge(other: Schedule[Env]) = {
    other.queue.headOption.foreach { firstEvent =>
      require(
        firstEvent.time >= time,
        "Events to merge already in the past"
      )
    }

    other.queue.foreach { event => queue.enqueue(event) }
  }

  /** Runs a schedule over an environment for n steps or until it is
    * exhausted.
    */
  def run(env: Env, n:Int = Int.MaxValue): Unit = {
    require(!queue.isEmpty, "Schedule already exhausted")

    while(runOneStep(env) && step < n) {
      // pass
    }
  }
}


object Schedule {
  val Epoch            = 0.0
  val BeforeSimulation = Epoch - 1.0
  val AfterSimulation  = Double.PositiveInfinity
  val MaximumTime      = 9.007199254740992E15

  // For values greater than 9.007199254740991E15, the epsilon is > 1.0.
  assert(
    ulp(9.007199254740991E15) <= 1.0 && ulp(9.007199254740992E15) > 1.0
  )

  def apply[Env](implicit rng: RNG): Schedule[Env] = new Schedule(
    BeforeSimulation, 0, rng
  )
}
