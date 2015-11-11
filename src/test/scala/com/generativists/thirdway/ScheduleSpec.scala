package com.generativists.thirdway

import com.generativists.thirdway.Implicits._

import org.apache.commons.math3.random.MersenneTwister

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.math.ulp
import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}


class ScheduleSpec extends FlatSpec with Matchers with BeforeAndAfter {
  import Schedule._
  type MyEnv   = ListBuffer[(Double, Int)]
  val rng      = new MersenneTwister()
  val schedule = Schedule[MyEnv](rng)

  class Appender(t: Double, o: Int) extends Activity[MyEnv] {
    def apply(env: MyEnv, schedule: Schedule[MyEnv]): Unit = {
      env.append(t -> o)
    }
  }

  before { schedule.reset() }

  "A Schedule" should "prioritize items by ActivationPoint" in {
    val times = List(1.0, 5.0, 100.0, 33.0)
    val orderings = List(5, 3, 1, 2)

    for(t <- times; o <- orderings) {
      schedule.enqueue(t, o, new Appender(t, o))
    }

    val testEnv = ListBuffer.empty[(Double, Int)]
    schedule.runUntilExhausted(testEnv)
    testEnv.toList shouldEqual testEnv.toList.sorted
  }

  it should "accept an event at the current time, but increment it" in {
    schedule.time = 1.0
    schedule.enqueue(1.0, 0, new NoOp[MyEnv])
    val scheduledTime = schedule.dequeueAll().head.time

    scheduledTime shouldNot equal(1.0)
    scheduledTime should be < 2.0
  }

  it should "not accent an event after MaximumTime" in {
    schedule.enqueue(1.0, 0, new NoOp[MyEnv])

    an [IllegalArgumentException] should be thrownBy {
      schedule.enqueue(MaximumTime + 2.0, 0, new NoOp[MyEnv]) shouldBe false
    }
  }

  it should "not accept an event prior to Epoch" in {
    an [IllegalArgumentException] should be thrownBy {
      schedule.enqueue(Epoch - ulp(Epoch), 0, new NoOp[MyEnv])
    }
  }

  it should "not accept an time that is NaN" in {
    an [IllegalArgumentException] should be thrownBy {
      schedule.enqueue(Double.NaN, 0, new NoOp[MyEnv])
    }
  }

  it should "not accept an event in the past" in {
    schedule.time = 10.0
    an [IllegalArgumentException] should be thrownBy {
      schedule.enqueue(5.0, 0, new NoOp[MyEnv])
    }
  }

  it should "remove and return all items on clear" in {
    val times = List(1.0, 5.0, 100.0, 33.0)
    val orderings = List(5, 3, 1, 2)
    for(t <- times; o <- orderings) {
      schedule.enqueue(t, 0, new NoOp[MyEnv])
    }

    val result = schedule.dequeueAll().map { event =>
      event.time -> event.order
    }.toList

    result shouldEqual result.sorted
  }

  it should "return to the original state on reset" in {
    schedule.time = 100.0
    schedule.step = 100
    (0 until 10) foreach { _ =>
      schedule.enqueue(100.0, 0, new NoOp[MyEnv])
    }
    schedule.length shouldEqual 10

    schedule.reset()
    schedule.length shouldEqual 0
    schedule.time shouldEqual Schedule.BeforeSimulation
    schedule.step shouldEqual 0
  }

  it should "shuffle events with equal times and orderings" in {
    type IntEnv = ListBuffer[Int]

    val items = (0 until 100) map { i =>
      (i, 0.0, 0, rng.nextInt)
    }

    val seed = rng.nextInt()
    rng.setSeed(seed)
    val envA = ListBuffer.empty[Int]
    val s = Schedule[IntEnv](rng)

    for((i, t,o,j) <- items) {
      s.enqueue(
        t, o,
        new Activity[IntEnv] {
          def apply(env: IntEnv, schedule: Schedule[IntEnv]): Unit = {
            env.append(i)
          }
        }
      )
    }
    s.runUntilExhausted(envA)

    s.reset()
    rng.setSeed(seed)
    val envB = ListBuffer.empty[Int]
    for((i, t,o,j) <- items) {
      s.enqueue(
        t, o,
        new Activity[IntEnv] {
          def apply(env: IntEnv, schedule: Schedule[IntEnv]): Unit = {
            env.append(i)
          }
        }
      )
    }
    s.runUntilExhausted(envB)
    envA shouldEqual envB

    s.reset()
    val envC = ListBuffer.empty[Int]
    for((i, t,o,j) <- items) {
      s.enqueue(
        t, o,
        new Activity[IntEnv] {
          def apply(env: IntEnv, schedule: Schedule[IntEnv]): Unit = {
            env.append(i)
          }
        }
      )
    }
    s.runUntilExhausted(envC)

    envA should not equal(envC)
  }

  it should "start running with a time equal to Epoch" in {
    val testEnv = ListBuffer.empty[(Double, Int)]

    schedule.enqueue(
      0.0, 0,
      (e: MyEnv, s: Schedule[MyEnv]) => {
        s.time shouldEqual Epoch
      }
    )

    schedule.runUntilExhausted(testEnv)


  }

  it should "execute activities with the same time ordered by order" in {
    schedule.enqueue(1.0, 1, new Appender(1, 1))
    schedule.enqueue(1.0, 0, new Appender(1, 0))
    schedule.enqueue(1.0, 0, new Appender(1, 0))
    schedule.enqueue(1.0, 2, new Appender(1, 2))

    val testEnv = ListBuffer.empty[(Double, Int)]
    schedule.runUntilExhausted(testEnv)
    testEnv shouldEqual testEnv.sorted
    schedule.step shouldEqual 1
  }

  it should "be clearable without reseting the time and step" in {
    schedule.time = 1.0
    schedule.step = 10
    schedule.enqueue(1.0, 1, new Appender(1, 1))

    schedule shouldNot be('empty)

    schedule.clear()
    schedule should be('empty)
    schedule.time shouldEqual 1.0
    schedule.step shouldEqual 10
  }

  it should "combine two schedules on a call to merge" in {
    val itemsA = (0 until 10) map { _ => rng.nextDouble -> rng.nextInt }
    for((t,o) <- itemsA) {
      schedule.enqueue(t, o, new Appender(t, o))
    }

    val itemsB = (0 until 10) map { _ => rng.nextDouble -> rng.nextInt }
    val scheduleB = Schedule[MyEnv](rng)
    for((t,o) <- itemsB) {
      scheduleB.enqueue(t, o, new Appender(t, o))
    }

    schedule.length shouldEqual 10
    scheduleB.length shouldEqual 10

    schedule.merge(scheduleB)
    schedule.length shouldEqual 20

    val testEnv = ListBuffer.empty[(Double, Int)]
    schedule.runUntilExhausted(testEnv)
    val bothSorted = (itemsA ++ itemsB).sorted
    testEnv shouldEqual bothSorted
  }

  it should "not merge over Events scheduled in the past" in {
    schedule.time  = 1
    schedule.step = 1

    schedule.enqueue(1, 1, new Appender(1, 1))
    val scheduleB = Schedule[MyEnv](rng)
    scheduleB.enqueue(0.5, 1, new Appender(0.5, 1))

    an [IllegalArgumentException] should be thrownBy {
      schedule.merge(scheduleB)
    }
  }

  it should "take functions given the implicit f2Activity conversion" in {
    val testEnv = ListBuffer.empty[(Double, Int)]

    schedule.enqueue(
      0.0, 0,
      (e: MyEnv, s: Schedule[MyEnv]) => e.append(2.0 -> 1)
    )

    schedule.runUntilExhausted(testEnv)

    testEnv shouldEqual ListBuffer(2.0 -> 1)
  }

  it should "allow for function scheduling without implicit conversion" in {
    val testEnv = ListBuffer.empty[(Double, Int)]

    schedule.once(1.0, 1) { (e, s) => e.append((s.time, 1)) }
    schedule.onceIn(2.0, 2) { (e, s) => e.append((s.time, 2)) }
    schedule.runUntilExhausted(testEnv)

    testEnv shouldEqual ListBuffer(0.0 -> 1, 1.0 -> 2)

    schedule.reset()


    //schedule.run(testEnv)
  }

  it should "allow an event to be scheduled once at a specific time" in {
    for((t,o) <- List(5.0 -> 1, 2.0 -> 2)) {
      schedule.once(new Appender(t, o), t, o)
    }

    val testEnv = ListBuffer.empty[(Double, Int)]
    schedule.runUntilExhausted(testEnv)

    testEnv shouldEqual testEnv.sorted
  }

  it should "allow an event to be scheduled some time delta ahead" in {
    schedule.time = 4.0

    for((t,o) <- List(6.0 -> 1, 2.0 -> 2)) {
      schedule.onceIn(new Appender(t, o), t, o)
    }

    val testEnv = ListBuffer.empty[(Double, Int)]

    schedule.runOneStep(testEnv)
    schedule.time shouldEqual 6.0 +- 0.0000001

    schedule.runOneStep(testEnv)
    schedule.time shouldEqual 10.0 +- 0.0000001
  }

  it should "scheduleOnce relative to Epoch not BeforeSimulation" in {
    schedule.onceIn(new Appender(1.0, 0), 1.0, 0)
    schedule.dequeueAll().head.time shouldEqual 1.0
  }

  it should "terminate after a number of steps if specified" in {
    (1 to 100) foreach { delta =>
      schedule.onceIn(new NoOp[MyEnv], delta.toDouble, 0)
    }

    schedule.runNSteps(ListBuffer.empty[(Double, Int)], 7)
    schedule.step shouldEqual 7
  }

  it should "run until stopped for event scheduled as repeating" in {
    val stoppable = schedule.repeating(
      new Appender(0.0, 1), 0.0, 5.0, 0
    )

    val testEnv = ListBuffer.empty[(Double, Int)]
    (1 to 20) foreach { step =>
      if (!schedule.isExhausted) {
        schedule.runOneStep(testEnv)
        step shouldEqual schedule.step

        if (step == 10) {
          stoppable.stop()
        }
      }
    }

    schedule should be('exhausted)
    schedule.step shouldEqual 11
    testEnv.length shouldEqual 10
  }

  it should "only allow repeating actions with a positive interval" in {
    an [IllegalArgumentException] should be thrownBy {
      val _ = schedule.repeating(new Appender(0.0, 1), 0.0, -1.0, 0)
    }
  }

  it should "raise an IllegalArgumentException if run after exhausted" in {
    val testEnv = ListBuffer.empty[(Double, Int)]
    schedule.onceIn(new NoOp[MyEnv], 1.0, 0)
    schedule.runUntilExhausted(testEnv)

    an [IllegalArgumentException] should be thrownBy {
      schedule.runUntilExhausted(testEnv)
    }
  }
}

class SequencedActivitiesSpec extends FlatSpec with Matchers {
  type MyEnv   = ListBuffer[Int]
  val rng      = new MersenneTwister()
  val schedule = Schedule[MyEnv](rng)

  class Appender(i: Int) extends Activity[MyEnv] {
    def apply(env: MyEnv, schedule: Schedule[MyEnv]): Unit = {
      env.append(i)
    }
  }

  "A SequencedActivity" should "execute each activity in order" in {
    val seq = SequencedActivities(
      List(new Appender(5), new Appender(2), new Appender(1), new Appender(9))
    )
    schedule.once(seq, 1.0, 0)
    val env = ListBuffer.empty[Int]
    schedule.runUntilExhausted(env)
    env shouldEqual ListBuffer(5, 2, 1, 9)
  }
}

class TentativeActivitySpec extends FlatSpec with Matchers {
  type MyEnv   = ListBuffer[Int]
  val rng      = new MersenneTwister()
  val schedule = Schedule[MyEnv](rng)

  class Appender(i: Int) extends Activity[MyEnv] {
    def apply(env: MyEnv, schedule: Schedule[MyEnv]): Unit = {
      env.append(i)
    }
  }

  "A TentativeActivity" should "not execute when stopped" in {
    schedule.once(TentativeActivity(new Appender(1)), 1.0, 0)
    val stoppable = TentativeActivity(new Appender(2))
    schedule.once(stoppable, 1.0, 0)
    val env = ListBuffer.empty[Int]
    stoppable.stop()
    schedule.runUntilExhausted(env)
    env shouldEqual ListBuffer(1)
  }
}

class ShuffledActivitiesSpec extends FlatSpec with Matchers {
  type MyEnv   = ListBuffer[Int]
  val rng      = new MersenneTwister()
  val schedule = Schedule[MyEnv](rng)

  class Appender(i: Int) extends Activity[MyEnv] {
    def apply(env: MyEnv, schedule: Schedule[MyEnv]): Unit = {
      env.append(i)
    }
  }

  "ShuffledActivities" should "run in an shuffled order" in {
    val activities = ShuffledActivities[MyEnv](
      (0 until 20).map{i => new Appender(i)}.toBuffer
    )

    val envA = ListBuffer.empty[Int]
    schedule.once(activities, 0.0, 1)
    schedule.runUntilExhausted(envA)

    val envB = ListBuffer.empty[Int]
    schedule.reset()
    schedule.once(activities, 0.0, 1)
    schedule.runUntilExhausted(envB)

    envA shouldNot equal(envB)
  }
}

