package com.generativists.thirdway.fields.grid2d


abstract class Grid[T:Manifest] {
  val width: Int
  val height: Int

  def apply(x: Int, y: Int): T
  def update(x: Int, y: Int, value: T): Unit
  def isInBounds(x: Int, y: Int) = true

  def tx(x: Int) = x
  def ty(y: Int) = y
}

trait WidthHeightBounding[T] extends Grid[T]{
  override def isInBounds(x: Int, y: Int) = {
    x >= 0 && x < width && y >= 0 && y < height
  }
}

trait Toroidal[T] extends Grid[T] {
  def apply(x: Int, y: Int): T
  def update(x: Int, y: Int, value: T): Unit
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



