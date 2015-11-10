package com.generativists

import org.apache.commons.math3.random.RandomGenerator
import scala.collection.mutable


package object thirdway {
  type RNG = RandomGenerator

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
