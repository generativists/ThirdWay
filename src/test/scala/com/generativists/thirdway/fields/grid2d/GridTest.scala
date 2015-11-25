package com.generativists.thirdway.fields.grid2d

import org.scalatest.{FunSpec, Matchers}


class GridTest extends FunSpec with Matchers {
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


class DenseGridTest extends FunSpec with Matchers {
  describe("DenseGrid") {
    it("should allow for updating and retrieving cell values") {
      val grid = new DenseGrid[Boolean](5, 3, false)

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

