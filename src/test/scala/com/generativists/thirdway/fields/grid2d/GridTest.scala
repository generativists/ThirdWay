package com.generativists.thirdway.fields.grid2d

import org.scalatest.{FunSpec, Matchers}


class GridSpec extends FunSpec with Matchers {
  val grid = new Grid[Int] {
    val width: Int = 10
    val height: Int = 20

    override def update(x: Int, y: Int, value: Int): Unit = {}
    override def apply(x: Int, y: Int): Int = -1
  }

  describe("Grid") {
    describe("isInBounds") {
      it("should check for coordinates in [0, width) and [0, height)") {
        List((0, 0), (5, 5), (9, 0), (0, 19), (9, 19)) foreach { coord =>
          grid.isInBounds(coord._1, coord._2) shouldBe true
        }

        List((-1, 0), (10, 0), (0, 20), (10, 20)) foreach { coord =>
          grid.isInBounds(coord._1, coord._2) shouldBe false
        }
      }
    }
    describe("tx ad ty") {
      it("should be simple identity functions") {
        grid.tx(10) shouldEqual 10
        grid.ty(3) shouldEqual 3
      }
    }
  }
}


class DenseGridSpec extends FunSpec with Matchers {
  describe("DenseGrid") {
    it("should allow for updating and retrieving cell values") {
      val grid = DenseGrid[Boolean](5, 3){ false }

      List((0,0), (1,1), (2,2), (3,1), (4,0)) foreach {
        case (x, y) => grid(x, y) = true
      }

      val result = grid.
        map { case ((x,y), value) => if (value) "x" else "o" }.
        mkString("").
        grouped(5).
        mkString("\n")

      result shouldEqual "xooox\noxoxo\nooxoo"
    }
  }
}

class ToroidalCoordinatesSpec extends FunSpec with Matchers {
  describe("SimpleToroidalCoordinates") {

    val grid = new DenseGrid(
      3, 2, false
    ) with SimpleToroidalCoordinates[Boolean]

    describe("isInBounds") {
      it("should be true if not more than one full dimension away") {
        List((-3, 0), (5, 0), (0, -2), (0, 3)) foreach {
          case (x,y) => grid.isInBounds(x, y) shouldBe true
        }

        List((-4, 0), (6, 0), (0, -3), (0, 4)) foreach {
          case (x,y) => grid.isInBounds(x, y) shouldBe false
        }
      }
    }

    describe("tx") {
      it("should wrap across the width") {
        List(0 -> 0, 1 -> 1, -3 -> 0, 2 -> 2, 5 -> 2) foreach {
          case (x, expectedX) => grid.tx(x) shouldEqual expectedX
        }
      }
    }

    describe("ty") {
      it("should wrap across the height") {
        List(0 -> 0, 1 -> 1, 2 -> 0, 3 -> 1, -1 -> 1, -2 -> 0) foreach {
          case (y, expectedY) => grid.ty(y) shouldEqual expectedY
        }
      }
    }
  }

  describe("RobustToroidalCoordinates") {

    val grid = new DenseGrid(
      3, 2, false
    ) with RobustToroidalCoordinates[Boolean]

    describe("isInBounds") {
      it("should always be true") {
        grid.isInBounds(2,1) shouldBe true
        grid.isInBounds(615, 1916) shouldBe true
        grid.isInBounds(-42, -42) shouldBe true
      }
    }

    describe("tx") {
      it("should wrap across the width") {
        List(0 -> 0, 1 -> 1, -6 -> 0, 2 -> 2, 8 -> 2, -5 -> 1) foreach {
          case (x, expectedX) => x->grid.tx(x) shouldEqual x->expectedX
        }
      }
    }

    describe("ty") {
      it("should wrap across the height") {
        List(0 -> 0, 1 -> 1, 2 -> 0, 5 -> 1, -1 -> 1, -4 -> 0) foreach {
          case (y, expectedY) => grid.ty(y) shouldEqual expectedY
        }
      }
    }
  }
}
