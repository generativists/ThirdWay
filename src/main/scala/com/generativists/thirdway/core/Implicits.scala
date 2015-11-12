package com.generativists.thirdway.core

import java.util.Random

import scala.util.Random

object Implicits {
  implicit def f2Activity[E, R](f: (E, Schedule[E]) => R): Activity[E] = {
    new Activity[E] {
      def apply(env: E, schedule: Schedule[E]): Unit = {
        val _ = f(env, schedule)
      }
    }
  }

  import scala.util.Random.javaRandomToRandom

  implicit def scalaRandom2JavaRandom(r: util.Random): java.util.Random = {
    r.self
  }
}
