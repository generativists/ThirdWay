package com.generativists.thirdway.fields.grid2d


/** Defines a neighborhood over a 2D Grid given an origin.*/
abstract class GridNeighbors {

  /** @return the neighbors' locations as a pair of Ints
    */
  def locations[T](x: Int, y: Int, grid: Grid[T]): Seq[(Int, Int)]

  /** @return the value in the grid for each neighbor */
  def of[T](
    x: Int, y: Int, grid: Grid[T]
  ): Seq[T] = locations(x, y, grid) map {
    case (x, y) => grid(x, y)
  }

  /** @return the location and value for each neighbor */
  def withLocations[T](
    x: Int, y: Int, grid: Grid[T]
  ): Seq[((Int,Int),T)] = locations(x, y, grid) map {
    point => point -> grid(point._1, point._2)
  }
}


/** Builds a Von Neumann Neighborhood.
  *
  * That is, a point is in the Von Neumann neighborhood if its Manhattan
  * distance is less than some given threshold. If the distance threshold
  * equals 1, then this is equivalent to {North, West, East, South}.
  */
class VonNeumann(val distance: Int) extends GridNeighbors {
  require(distance > 0, "Distance must be positive")

  /** @note The returned sequence has a guaranteed ordering. It enumerates
    *       locations in the coordinate style common to computer graphics.
    *       That is, North West is (0, 0) and South East is (N, N). It
    *       proceeds to East-ward (across rows) first, then moves one unit
    *       South (down one column). This may not have any value in the future.
    *       However, I think it may make drawing operations easier.
    */
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


/** Filters out points are out-of-bounds according to the Grid.*/
trait Boundaries extends GridNeighbors {
  abstract override def locations[T](
    x0: Int, y0: Int, grid: Grid[T]
  ) = super.locations(x0, y0, grid) filter { point =>
    grid.isInBounds(point._1, point._2)
  }
}

/**
  * @see http://www.redblobgames.com/grids/hexagons/
  */
class Hexagonal extends GridNeighbors {
  def north(x: Int, y: Int)     = (x, y - 1)
  def northWest(x: Int, y: Int) = (x - 1) -> (if (x % 2 == 0) y - 1 else y)
  def northEast(x: Int, y: Int) = (x + 1) -> (if (x % 2 == 0) y - 1 else y)
  def southWest(x: Int, y: Int) = (x - 1) -> (if (x % 2 == 0) y else y + 1)
  def southEast(x: Int, y: Int) = (x + 1) -> (if (x % 2 == 0) y else y + 1)
  def south(x: Int, y: Int)     = (x, y + 1)

  def locations[T](x: Int, y: Int, grid: Grid[T]): Seq[(Int, Int)] = {
    if (x % 2 == 0) {
      Seq((x, y-1), (x-1, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x, y+1))
    } else {
      Seq((x, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x+1, y+1), (x, y+1))
    }
  }
}
