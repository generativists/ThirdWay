package com.generativists.thirdway

object Implicits {
  implicit def f2Activity[E, R](f: (E, Schedule[E]) => R): Activity[E] = {
    new Activity[E] {
      def apply(env: E, schedule: Schedule[E]): Unit = {
        val _ = f(env, schedule)
      }
    }
  }
}
