package com.generativists.thirdway

import scala.math.ulp
import scala.collection.mutable

class Schedule[Env] (
  var time: Double,
  var steps: Long,
  val rng: RNG
) extends Serializable {
  val queue = mutable.PriorityQueue[Event[Env]]()(Ordering[Event[Env]].reverse)

  val shufflerTmp = mutable.ArrayBuffer.empty[Event[Env]]

  def length = queue.length

  def isEmpty = queue.isEmpty

  def isComplete = isEmpty

  def isExhausted = steps != 0 && isEmpty

  def reset() = {
    time = Schedule.BeforeSimulation
    steps = 0
    queue.clear()
  }

  def clear() = queue.dequeueAll

  def enqueue(at: Double, order: Int = 0, action: Activity[Env]): Boolean = {
    if (at >= Schedule.AfterSimulation) return false

    require(!at.isNaN, "Scheduled at NaN")
    require(at >= Schedule.Epoch, "Scheduled before Epoch")
    require(at >= time, "Scheduled in the past")

    if(time == at) {
      queue.enqueue(Event(at, order, action))
    } else {
      queue.enqueue(Event(at + ulp(at), order, action))
    }

    return true
  }

  def once(action: Activity[Env], time: Double, order: Int = 0): Boolean = {
    enqueue(time, order, action)
  }

  def onceIn(action: Activity[Env], delta: Double, order: Int = 0): Boolean = {
    enqueue(time + delta, order, action)
  }

  def repeating(
    action: Activity[Env],
    startAt: Double=1.0,
    interval: Double=1.0,
    order: Int=0
  ): Stoppable[Env] = {
    require(interval > 0)
    val activity = RepeatingActivity(action, interval, order)
    enqueue(startAt, order, activity)
    activity
  }

  def step(env: Env): Boolean = {
    if(time == Schedule.AfterSimulation || queue.isEmpty) {
      return false
    }

    // Gather
    shufflerTmp.clear()
    var continue = true
    val matcher = queue.head
    while(continue) {
      shufflerTmp.append(queue.dequeue())

      continue = !isEmpty && matcher.compare(queue.head) == 0
    }

    Util.shuffleInPlace(rng, shufflerTmp)

    // Run.
    shufflerTmp foreach { _.activity(env, this) }

    time = matcher.time
    steps += 1

    return true
  }

  def merge(other: Schedule[Env]) = {
    other.queue.headOption.foreach { firstEvent =>
      require(
        firstEvent.time >= time,
        "Events to merge already in the past"
      )
    }

    other.queue.foreach { event => queue.enqueue(event) }
  }

  def run(env: Env, n:Int = Int.MaxValue) = {
    require(!queue.isEmpty, "Schedule already exhausted")

    while(step(env) && steps < n) {

    }

    this
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
