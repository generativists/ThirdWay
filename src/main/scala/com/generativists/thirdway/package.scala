package com.generativists

import org.apache.commons.math3.random.RandomGenerator
import scala.language.implicitConversions
import scala.collection.mutable


package object thirdway {
  type RNG = RandomGenerator

  implicit def f2Activity[E, R](f: (E, Schedule[E]) => R): Activity[E] = {
    new Activity[E] {
      def apply(env: E, schedule: Schedule[E]): Unit = {
        val _ = f(env, schedule)
      }
    }
  }

  /** Shuffles a sequence in-place.
    *
    * @see The *Knuth* shuffle implementation of `scala.util.Random`.
    */
  def shuffleInPlace[T](rng: RNG, items: mutable.Seq[T]): Unit = {
    for (n <- items.length to 2 by -1) {
      val i = n - 1
      val j = rng.nextInt(n)

      val tmp = items(i)
      items(i) = items(j)
      items(j) = tmp
    }
  }
}
