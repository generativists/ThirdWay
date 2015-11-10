package com.generativists.thirdway

import java.io.{ObjectInputStream, ByteArrayInputStream, ObjectOutputStream, ByteArrayOutputStream}

import scala.collection.mutable

object Util {
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

  /** Clones a object via serialization. */
  def serializingClone[T](obj: T): T = {
    val byteStream = new ByteArrayOutputStream()
    new ObjectOutputStream(byteStream).writeObject(obj)

    new ObjectInputStream(
      new ByteArrayInputStream(byteStream.toByteArray())
    ).readObject().asInstanceOf[T]
  }
}
