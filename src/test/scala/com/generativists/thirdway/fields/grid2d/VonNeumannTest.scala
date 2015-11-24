package com.generativists.thirdway.fields.grid2d

import org.scalatest.{FunSpec, Matchers}


class MockGrid extends Grid[Int] with WidthHeightBounding[Int] {
  val width: Int = 3
  val height: Int = 3

  override def update(x: Int, y: Int, value: Int): Unit = { }

  override def apply(x: Int, y: Int): Int = x + y
}


class VonNeumannSpec extends FunSpec with Matchers {
  describe("VonNeumann") {
    describe("neighborIndices") {
      it("should return a diamond around some origin") {
        VonNeumann.neighborIndices(1, 1, 2) shouldEqual List(
                           (1, -1),
                   (0, 0), (1,  0), (2, 0),
          (-1, 1), (0, 1),          (2, 1), (3, 1),
                   (0, 2), (1,  2), (2, 2),
                           (1,  3)
        )
      }
    }

    describe("raises an IllegalArgumentException if distance < 1") {
      an [IllegalArgumentException] should be thrownBy {
        val _ = new VonNeumann(0)
      }
      an [IllegalArgumentException] should be thrownBy {
        val _ = new VonNeumann(-1)
      }
    }

    describe("of") {
      it("should returns the values in each neighbor cell") {
        val neighbors = new VonNeumann(2)

        neighbors.of(1, 1, new MockGrid()) shouldEqual List(
          0, 0, 1, 2, 0, 1, 3, 4, 2, 3, 4, 4
        )
      }
    }

    describe("withLocations") {
      it("should returns the point and value of each neighbor") {
        val neighbors = new VonNeumann(2)

        neighbors.withLocations(1, 1, new MockGrid()) shouldEqual List(
          (1, -1) -> 0,
          (0, 0) -> 0, (1, 0) -> 1, (2, 0) -> 2,
          (-1, 1) -> 0, (0, 1) -> 1, (2, 1) -> 3, (3, 1) -> 4,
          (0, 2) -> 2, (1, 2) -> 3, (2, 2) -> 4,
          (1, 3) -> 4
        )
      }
    }
  }
}


class BoundariesSpec extends FunSpec with Matchers {
  describe("A Neighborhood with Boundaries") {
    describe("locations") {
      it("should filter neighbors that are out of bounds") {
        val neighbors = new VonNeumann(2) with Boundaries

        neighbors.locations(1, 1, new MockGrid()) shouldEqual List(
                            // (1, -1),
                       (0, 0), (1,  0), (2, 0),
          /*(-1, 1),*/ (0, 1),          (2, 1), // (3, 1)
                       (0, 2), (1,  2), (2, 2)
                           //  (1,  3)
        )
      }
    }
  }
}
