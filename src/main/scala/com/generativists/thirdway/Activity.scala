package com.generativists.thirdway

import scala.collection.mutable

trait Activity[Env] extends Serializable {
  /** Run this activity over some environment and given a schedule.*/
  def apply(env: Env, schedule: Schedule[Env]): Unit
}

trait Stoppable[Env] extends Activity[Env] {
  /** Prevent this activity from running once dequeued from the schedule */
  def stop(): Unit
}

/** An `Activity` that will run once, but only if it has not been `stop()`ed.
  *
  * @note Calling `stop` does not remove the `TentativeActivity` from the
  *       schedule. Instead, once `dequeued`, it simply fails to take any
  *       action on `apply(env, schedule)`.
  *
  * @note The initial implementation used had `activity: Option[Activity]`.
  *       Calling `stop` set this field to `None`. It felt like proper Scala.
  *       However, it also allows for substitution of an activity, which is
  *       wrong. An immutable activity is better.
  * */
case class TentativeActivity[Env] (
  activity: Activity[Env],
  private var stopped: Boolean = false
) extends Stoppable[Env] {
  def stop() = stopped = true

  def apply(env: Env, schedule: Schedule[Env]): Unit = if(!stopped) {
    activity(env, schedule)
  }
}

/** An activity that reschedules itself indefinitely, at a fixed interval.
  *
  * @see the implementation note in `TentativeActivity`*/
case class RepeatingActivity[Env](
  activity: Activity[Env],
  interval: Double,
  order: Int,
  private var stopped: Boolean = false
) extends Stoppable[Env] {
  def stop() = { stopped = true }

  def apply(env: Env, schedule: Schedule[Env]): Unit = {
    if (!stopped) {
      activity(env, schedule)
      schedule.onceIn(this, interval, order)
    }
  }
}

/** Runs a sequence of activities in a fixed order. */
case class SequencedActivities[Env](
  activities: Seq[Activity[Env]]
) extends Activity[Env] {
  def apply(env: Env, schedule: Schedule[Env]): Unit = activities.foreach {
    activity => activity(env, schedule)
  }
}

/** Runs a set of activities in randomized order. */
case class ShuffledActivities[Env](
  activities: mutable.Seq[Activity[Env]]
) extends Activity[Env] {

  /** @note This method uses the ``scheduler`s RNG as a source. */
  def apply(env: Env, schedule: Schedule[Env]): Unit = {
    Util.shuffleInPlace(schedule.rng, activities)
    activities foreach { activity => activity(env, schedule) }
  }

}

/** Does nothing. Useful for testing */
class NoOp[Env] extends Activity[Env] {
  def apply(env: Env, schedule: Schedule[Env]): Unit = {}
}
