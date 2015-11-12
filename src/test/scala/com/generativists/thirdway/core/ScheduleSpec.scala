package com.generativists.thirdway.core

import scala.collection.mutable.ListBuffer
import scala.math.ulp

class ScheduleSpec extends ScheduleComponentSpec {
  import Schedule._

  describe("A Schedule") {
    it("should prioritize Events by ascending (time, group) order") {
      val (env, schedule) = genListBufferBasedEnvAndSchedule[(Double, Int)]()

      for(time <- List(1.0, 5.0, 100.0, 33.0); group <- List(5, 3, 1, 2)) {
        schedule.once(time, group) { (buf, _) => buf.append(time -> group) }
      }

      schedule.runUntilExhausted(env)
      env.toList shouldEqual env.toList.sorted
    }

    describe("once") {
      describe("when called with time equal to the schedule's time") {
        it("increments the given time by Double's epsilon") {
          val now = 1.0

          val schedule = genSchedule[Int]()
          schedule.time = now

          schedule.once(new NoOp[Int], now, DefaultGroup)

          schedule.peek.time shouldNot equal(now)
          schedule.peek.time shouldEqual now + ulp(now)
        }
      }

      describe("should raise an IllegalArgumentException") {
        it("when the time is NaN") {
          an [IllegalArgumentException] should be thrownBy {
            genSchedule[Int]().once(new NoOp[Int], Double.NaN, DefaultGroup)
          }
        }

        it("when the event time is prior to Epoch") {
          an [IllegalArgumentException] should be thrownBy {
            genSchedule[Int]().once(new NoOp[Int], Epoch - 100.0, DefaultGroup)
          }
        }

        it("when the event time is in the past relative to schedule time") {
          val schedule = genSchedule[Int]()
          schedule.time = 10.0
          an [IllegalArgumentException] should be thrownBy {
            schedule.once(new NoOp[Int], 5.0, DefaultGroup)
          }
        }
      }
    }

    describe("onceIn") {
      it("should schedule an event to activate some time delta ahead") {
        val (env, schedule) = genListBufferBasedEnvAndSchedule[Int]()
        val now = 4.0
        schedule.time = now

        for((t, g) <- List(6.0 -> 1, 2.0 -> 2)) {
          schedule.onceIn(t, g) { (_, s) => s.time shouldEqual t + now }
        }
        schedule.runUntilExhausted(env)
      }

      it("should schedule relative to Epoch if time is BeforeSimulation") {
        val schedule = genSchedule[Int]()
        schedule.onceIn(new NoOp[Int], 1.0, 0)
        schedule.peek.time shouldEqual 1.0
      }

      it("should schedule relative to the time if >= Epoch") {
        val schedule = genSchedule[Int]()
        schedule.time = 10.0
        schedule.onceIn(1.0, 0) { (e, s) => s.time shouldEqual 11.0}
        schedule.runUntilExhausted(10)
      }

      it("should raise an IllegalArgumentException if delta < Double's ε") {
        val schedule = genSchedule[Int]()
        schedule.time = MaximumInteger
        an [IllegalArgumentException] should be thrownBy {
          schedule.onceIn(new NoOp[Int], 1.0, DefaultGroup)
        }
      }
    }

    describe("repeating") {
      it("should raise an IllegalArgumentException for a negative interval") {
        val schedule = genSchedule[Int]()

        an [IllegalArgumentException] should be thrownBy {
          val _ = schedule.repeating(new NoOp[Int], 0.0, -1.0, 0)
        }
      }

      it("should reschedule itself and run until it is stopped") {
        val (env, schedule) = genListBufferBasedEnvAndSchedule[(Double, Int)]()

        val stoppable = schedule.repeating(0.0, 5.0, 0) { (e, s) =>
          e.append(0.0 -> 1)
        }

        (1 to 20) foreach { step =>
          if (!schedule.isExhausted) {
            schedule.runOneStep(env)
            step shouldEqual schedule.step

            if (step == 10) { stoppable.stop() }
          }
        }

        schedule should be('exhausted)
        schedule.step shouldEqual 11
        env.length shouldEqual 10
      }
    }

    describe("peek") {
      it("should return highest priority event without dequeuing it") {
        val schedule = genSchedule[Int]()
        schedule.once(new NoOp[Int], 1.0, 1)
        schedule.once(new NoOp[Int], 1.0, 0)
        schedule.length shouldEqual 2

        schedule.peek.time shouldEqual 1.0
        schedule.peek.group shouldEqual 0
        schedule.length shouldEqual 2
      }
    }

    describe("clear") {
      it("should only remove all the items from the queue") {
        val schedule = genSchedule[Int]
        schedule.time = 5.0
        schedule.step = 1

        schedule.once(new NoOp[Int], 6.0, DefaultGroup)
        schedule shouldNot be('empty)

        schedule.clear()

        schedule should be('empty)
        schedule.time shouldEqual 5.0
        schedule.step shouldEqual   1
      }
    }

    describe("reset") {
      it("should reset all state variables to their prestine values") {
        val schedule = genSchedule[Int]()

        def testPrestine(): Unit = {
          schedule.time   shouldEqual Schedule.BeforeSimulation
          schedule.step   shouldEqual 0
          schedule.length shouldEqual 0
        }

        testPrestine()

        schedule.time = 75.0
        schedule.step = 50
        (0 until 10) foreach { i =>
          schedule.once(new NoOp[Int], 100.0, DefaultGroup)
        }
        schedule.length shouldEqual 10

        schedule.reset()
        testPrestine()
      }
    }

    describe("dequeueAll") {
      it("should remove and return all events from the queue in order") {
        val schedule = genSchedule[ListBuffer[(Double, Int)]]()

        for(time <- List(1.0, 5.0, 100.0, 33.0); group <- List(5, 3, 1, 2)) {
          schedule.once(new NoOp[ListBuffer[(Double, Int)]], time, group)
        }

        val result = schedule.dequeueAll().map { event =>
          event.time -> event.group
        }.toList

        result shouldEqual result.sorted
      }
    }

    describe("merge") {
      it("should add events from the argument schedule to the called one") {
        val itemsA = (0 until 10) map { _ => rng.nextDouble -> rng.nextInt }
        val itemsB = (0 until 5) map { _ => rng.nextDouble -> rng.nextInt }

        val scheduleA = genSchedule[ListBuffer[(Double, Int)]]()
        val scheduleB = genSchedule[ListBuffer[(Double, Int)]]()

        itemsA foreach { case (t, g) =>
          scheduleA.once(t, g) { (e, s) => e.append(t -> g) }
        }
        itemsB foreach { case (t, g) =>
          scheduleB.once(t, g) { (e, s) => e.append(t -> g) }
        }

        scheduleA.length shouldEqual 10
        scheduleB.length shouldEqual 5

        scheduleA.merge(scheduleB)
        scheduleA.length shouldEqual 15
        scheduleB.length shouldEqual 5

        val env = ListBuffer.empty[(Double, Int)]
        scheduleA.runUntilExhausted(env)
        env shouldEqual (itemsA ++ itemsB).sorted
      }

      it("should raise an IllegalArgumentExcept if merging events from past") {
        val scheduleA = genSchedule[Int]()
        val scheduleB = genSchedule[Int]()

        scheduleA.time = 5.0
        scheduleB.once(new NoOp[Int], 1.0, DefaultGroup)

        an [IllegalArgumentException] should be thrownBy {
          scheduleA.merge(scheduleB)
        }
      }
    }

    it ("should have an human-readable toString for debugging purposes") {
      val examples = List(
        // Time, Step, Activities, Expected String
        (
          BeforeSimulation, 0L, List.empty[NoOp[Int]],
          "Schedule at time=BeforeSimulation, step=0 with no queued activities"
        ),(
          Epoch, 0L, List.empty[NoOp[Int]],
          "Schedule at time=Epoch, step=0 with no queued activities"
        ),(
          AfterSimulation, 0L, List.empty[NoOp[Int]],
          "Schedule at time=AfterSimulation, step=0 with no queued activities"
        ),(
          Epoch, 0L, List(new NoOp[Int]),
          "Schedule at time=Epoch, step=0 with one queued activity"
        ),(
          Epoch, 1L, List(new NoOp[Int], new NoOp[Int]),
          "Schedule at time=Epoch, step=1 with 2 queued activities"
        ),(
          1.0, 1L, List.empty[NoOp[Int]],
          "Schedule at time=1.0, step=1 with no queued activities [Exhausted]"
        )
      )

      val schedule = genSchedule[Int]()
      for((time, step, activities, expectation) <- examples) {
        schedule.reset()
        schedule.time = time
        schedule.step = step
        activities foreach { schedule.once(_, time, DefaultGroup) }
        schedule.toString() shouldEqual expectation
      }
    }

    describe("run family") {
      it("should set the time to Epoch if never ran before") {
        val (env, schedule) = genListBufferBasedEnvAndSchedule[Int]()
        schedule.once(0.0, DefaultGroup) { (e, s) =>
          s.time shouldEqual Epoch
        }
        schedule.runUntilExhausted(env)
      }

      it("should raise an IllegalArgumentException if run when exhausted") {
        val (env, schedule) = genListBufferBasedEnvAndSchedule[Int]()

        schedule.onceIn(new NoOp[ListBuffer[Int]], 1.0, 0)
        schedule.runUntilExhausted(env)

        an [IllegalArgumentException] should be thrownBy {
          schedule.runUntilExhausted(env)
        }
      }
    }

    describe("runNSteps") {
      it("should terminate after nSteps even if not exhausted") {
        val (env, schedule) = genListBufferBasedEnvAndSchedule[Int]()

        (1 to 100) foreach { delta =>
          schedule.onceIn(new NoOp[ListBuffer[Int]], delta.toDouble, 0)
        }

        schedule.runNSteps(env, 7)
        schedule.step shouldEqual 7
      }
    }

    describe("runOneStep") {
      it("should shuffle events with equal times and orderings") {
        val items = (0 until 100) map { i => (i, 0.0, DefaultGroup) }

        // First run.
        val (envA, scheduleA) = genListBufferBasedEnvAndSchedule[Int]()
        val seed = scheduleA.rng.nextLong()
        scheduleA.rng.setSeed(seed)
        items foreach { case (i, t, g) =>
          scheduleA.once(t, g) { (env, s) => env.append(i) }
        }
        scheduleA.runUntilExhausted(envA)

        // Second run with same seed.
        val (envB, scheduleB) = genListBufferBasedEnvAndSchedule[Int]()
        rng.setSeed(seed)
        items foreach { case (i, t, g) =>
          scheduleB.once(t, g) { (env, s) => env.append(i) }
        }
        scheduleB.runUntilExhausted(envB)
        envA shouldEqual envB  // Same seed.

        // Third run, rng not reseeded.
        val (envC, scheduleC) = genListBufferBasedEnvAndSchedule[Int]()
        items foreach { case (i, t, g) =>
          scheduleC.once(t, g) { (env, s) => env.append(i) }
        }
        scheduleC.runUntilExhausted(envC)
        envC shouldNot equal(envA)  // Different starting seed
      }

      it("should execute activities with the same time in group order") {
        val (env, schedule) = genListBufferBasedEnvAndSchedule[(Int, Int)]()

        schedule.once(1.0, 1) { (e, s) => e.append(1 -> 1) }
        schedule.once(1.0, 0) { (e, s) => e.append(1 -> 0) }
        schedule.once(1.0, 0) { (e, s) => e.append(1 -> 0) }
        schedule.once(1.0, 2) { (e, s) => e.append(1 -> 2) }

        schedule.runUntilExhausted(env)
        env shouldEqual env.sorted
        schedule.step shouldEqual 1
      }
    }
  }
}

