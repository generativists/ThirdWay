package com.generativists.thirdway.core

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest.{Matchers, FunSpec}

import scala.collection.mutable.ListBuffer

abstract class ScheduleComponentSpec extends FunSpec with Matchers {
  val rng = new MersenneTwister()

  def genSchedule[T]() = Schedule[T](rng)

  def genListBufferBasedEnvAndSchedule[T]() = {
    val env      = ListBuffer.empty[T]
    val schedule = genSchedule[ListBuffer[T]]()

    (env, schedule)
  }
}
