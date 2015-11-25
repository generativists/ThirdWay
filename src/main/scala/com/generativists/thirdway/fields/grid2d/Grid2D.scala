package com.generativists.thirdway.fields.grid2d


abstract class Grid[T:Manifest] {
  val width: Int
  val height: Int

  def apply(x: Int, y: Int): T

  def update(x: Int, y: Int, value: T): Unit

  def isInBounds(x: Int, y: Int) = {
    x >= 0 && x < width && y >= 0 && y < height
  }

  def tx(x: Int): Int = x
  def ty(y: Int): Int = y
}

/** Endows a Grid with a toroidal surface.
  *
  * @note I adopted this code from MASON's `Grid2D` `stx` and `sty` methods. It
  * skips modulo for untranslated out-of-bounds, but has a defined domain. I
  * don't think I've ever seen a simulation that needs translations a fill
  * width or height off the array. For the overwhelming majority of use cases,
  * this is the appropriate trait.
  *
  * @note I coded the domain in `isInBounds` as an elidable requirement. Most
  *       simulations can prove compliance. For example, all accesses go
  *       through `Neighbors` which has a distance less than both the width
  *       and height. Neither `tx` nor `ty` do domain checking. Violations to
  *       the domain should fail loudly though. Inevitably, they should throw
  *       an `ArrayIndexOutOfBoundsException`.
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
    require(
      x >= -width && x < width * 2 && y >= -height && y < height * 2,
      s"($x,$y) is out of the domain given by ($width, $height)"
    )
    true
  }
}

trait RobustToroidalCoordinates[T] extends Grid[T] {

  override def tx(x: Int): Int = {
    if(x >= 0) {
      if(x < width) x else x % width
    } else {
      val foldedX = x % width
      if (foldedX < 0) foldedX + width else x
    }
  }

  override def ty(y: Int): Int = {
    if(y >= 0) {
      if(y < height) y else y % height
    } else {
      val foldedY = y % height
      if (foldedY < 0) foldedY + height else y
    }
  }

  override def isInBounds(x: Int, y: Int) = true
}

class DenseGrid2D[T:Manifest](
  val width: Int,
  val height: Int,
  val initialValue: T
) extends Grid[T] {

  protected val cells = Array.fill(height, width) { initialValue }

  def apply(x: Int, y: Int) = cells(tx(x))(ty(y))

  def update(x: Int, y: Int, value: T) = cells(tx(x))(ty(y)) = value
  
}



