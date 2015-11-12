package com.generativists.thirdway.core

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._


class TestAppender(i: Int) extends Activity[ListBuffer[Int]]{
  def apply(e: ListBuffer[Int], s: Schedule[ListBuffer[Int]]): Unit = {
    e.append(i)
  }
}


class RepeatingActivitySpec extends ScheduleComponentSpec {
  describe("RepeatingActivity") {
    it("should run its activity then reschedule itself at time + interval") {
      val (env, schedule) = genListBufferBasedEnvAndSchedule[Int]()
      schedule.time = 1.0

      val repeater = RepeatingActivity(new TestAppender(1), 1.0, 3)
      repeater(env, schedule)

      schedule.peek.time shouldEqual 2.0
      schedule.peek.group shouldEqual 3
    }
  }
}


class SequencedActivitiesSpec extends ScheduleComponentSpec {
  describe("A SequencedActivity") {
    it("should execute each activity in the given, static order") {
      val (env, schedule) = genListBufferBasedEnvAndSchedule[Int]()

      val activity = SequencedActivities(
        List(5, 2, 1, 9) map { new TestAppender(_) }
      )

      activity(env, schedule)
      env shouldEqual ListBuffer(5, 2, 1, 9)
    }
  }
}

class TentativeActivitySpec extends ScheduleComponentSpec {
  describe("A TentativeActivity") {
    describe("after calling stop") {
      it("should do nothing when called") {
        val (env, schedule) = genListBufferBasedEnvAndSchedule[Int]()

        schedule.once(TentativeActivity(new TestAppender(1)), 1.0, 0)

        val stoppable = TentativeActivity(new TestAppender(2))
        schedule.once(stoppable, 1.0, 0)
        stoppable.stop()

        schedule.runUntilExhausted(env)
        env shouldEqual ListBuffer(1)
      }
    }
  }
}

class ShuffledActivitiesSpec extends ScheduleComponentSpec {
  describe("ShuffledActivities") {
    it("should run in an shuffled order each time") {
      val (envA, schedule) = genListBufferBasedEnvAndSchedule[Int]()
      val envB = ListBuffer.empty[Int]

      val activities = ShuffledActivities[ListBuffer[Int]](
        (0 until 20).map { i => new TestAppender(i) }.toBuffer
      )

      schedule.once(activities, 0.0, 1)
      schedule.runUntilExhausted(envA)

      schedule.reset()
      schedule.once(activities, 0.0, 1)
      schedule.runUntilExhausted(envB)

      envA shouldNot equal(envB)  // Very unlikely. 12! is a big number.
    }
  }
}

class LocallyParallelActivitySpec extends ScheduleComponentSpec {
  describe("A LocallyParallelActivity") {
    it("should execute activities across all cores") {
      type MyEnv = java.util.concurrent.LinkedBlockingQueue[Int]
      val schedule = genSchedule[MyEnv]()
      val env = new java.util.concurrent.LinkedBlockingQueue[Int]()


      // This is basically a time-sort.
      // However, it may be a fragile test in different environments.
      // I really don't like this test.
      val activities = (1 to 8) map { i =>
        new Activity[MyEnv] {
          override def apply(e: MyEnv, s: Schedule[MyEnv]): Unit = {
            Thread.sleep((8L - i) * 100L)
            val _ = e.add(i)
          }
        }
      }
      schedule.once(LocallyParallelActivity(activities), 1.0, 0)
      schedule.runUntilExhausted(env)

      env.toList shouldEqual List(8, 7, 6, 5, 4, 3, 2, 1)
    }
  }
}



