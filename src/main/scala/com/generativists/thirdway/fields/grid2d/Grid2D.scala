package com.generativists.thirdway.fields.grid2d


abstract class Grid[T:Manifest] extends Traversable[((Int,Int),T)]{
  val width: Int
  val height: Int

  def apply(x: Int, y: Int): T

  def update(x: Int, y: Int, value: T): Unit

  def isInBounds(x: Int, y: Int) = {
    x >= 0 && x < width && y >= 0 && y < height
  }

  def tx(x: Int): Int = x
  def ty(y: Int): Int = y

  def foreach[U](f: (((Int,Int),T)) => U): Unit = {
    for(
      y <- Range(0, height);
      x <- Range(0, width)
    ) {
      f((x,y) -> this(x,y))
    }
  }
}

/** Endows a Grid with a toroidal surface.
  *
  * @note I adopted this code from MASON's `Grid2D` `stx` and `sty` methods. It
  * skips modulo for untranslated out-of-bounds, but has a defined domain. I
  * don't think I've ever seen a simulation that needs translations a fill
  * width or height off the array. For the overwhelming majority of use cases,
  * this is the appropriate trait.
  */
trait SimpleToroidalCoordinates[T] extends Grid[T] {

  override def tx(x: Int) = if (x >= 0) {
    if (x < width) x else x - width
  } else {
    x + width
  }

  override def ty(y: Int) = if (y >= 0) {
    if (y < height) y else y - height
  } else {
    y + height
  }

  override def isInBounds(x: Int, y: Int): Boolean = {
    x >= -width && x < width * 2 && y >= -height && y < height * 2
  }
}

trait RobustToroidalCoordinates[T] extends Grid[T] {

  override def tx(x: Int): Int = {
    if(x >= 0 && x < width) {
      x
    } else {
      val foldedX = x % width
      if (foldedX < 0) foldedX + width else foldedX
    }
  }

  override def ty(y: Int): Int = {
    if(y >= 0 && y < height) {
      y
    } else {
      val foldedY = y % height
      if (foldedY < 0) foldedY + height else foldedY
    }
  }

  override def isInBounds(x: Int, y: Int) = true
}

class DenseGrid[T:Manifest](
  val width: Int,
  val height: Int,
  initializer: => T
) extends Grid[T] {

  protected val cells = Array.fill(height, width) { initializer }

  def apply(x: Int, y: Int) = cells(ty(y))(tx(x))

  def update(x: Int, y: Int, value: T) = cells(ty(y))(tx(x)) = value

}

object DenseGrid {
  def apply[T:Manifest](width: Int, height: Int)(elem: => T): DenseGrid[T] = {
    new DenseGrid[T](width, height, elem)
  }
}



