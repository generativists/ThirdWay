package com.generativists.thirdway.fields.grid2d


abstract class GridNeighbors {
  def locations[T](x: Int, y: Int, grid: Grid[T]): Seq[(Int, Int)]

  def of[T](
    x: Int, y: Int, grid: Grid[T]
  ): Seq[T] = locations(x, y, grid) map {
    case (x, y) => grid(x, y)
  }

  def withLocations[T](
    x: Int, y: Int, grid: Grid[T]
  ): Seq[((Int,Int),T)] = locations(x, y, grid) map {
    point => point -> grid(point._1, point._2)
  }
}


class VonNeumann(val distance: Int) extends GridNeighbors {
  require(distance > 0, "Distance must be positive")

  override def locations[T](x0: Int, y0: Int, grid: Grid[T]) = {
    VonNeumann.neighborIndices(x0, y0, distance)
  }
}

object VonNeumann {
  def neighborIndices(x0: Int, y0: Int, distance: Int): Seq[(Int, Int)] = for(
    y <- Range(y0-distance, y0 + distance + 1);
    x <- Range(x0-distance, x0 + distance + 1)
    if ((x0 - x).abs + (y0 - y).abs) <= distance  // Within the neighborhood
      && !(x == x0 && y == y0)                    // Not the origin
  ) yield (x, y)
}


trait Boundaries extends GridNeighbors {
  abstract override def locations[T](
    x0: Int, y0: Int, grid: Grid[T]
  ) = super.locations(x0, y0, grid) filter { point =>
    grid.isInBounds(point._1, point._2)
  }
}
