package Cgl

import scala.io.StdIn

object Cgl2A {

  case class Point(x: Double = 0, y: Double = 0) {

    def +(p: Point): Point = Point(this.x + p.x, this.y + p.y)

    def -(p: Point): Point = Point(this.x - p.x, this.y - p.y)

    def *(a: Double): Point = Point(this.x * a, this.y * a)

    def /(a: Double): Point = {
      assert(a == 0)
      Point(this.x / a, this.y / a)
    }

    def norm(): Double = this.x * this.x + this.y * this.y

    def abs(): Double = scala.math.sqrt(norm())

    def <(p: Point): Boolean = this.x < p.x && this.y < p.y

    private val EPS = 1 ^ (-10)

    def ==(p: Point): Boolean = scala.math.abs(this.x - p.x) < EPS && scala.math.abs(this.y - p.y) < EPS

  }

  case class Segment(p1: Point, p2: Point)

  type GeoVector = Point

  object Cgl0A {

    // 内積
    def dot(a: Point, b: Point): Double = a.x * b.x + a.y * b.y

    def isOrthogonal(a: GeoVector, b: GeoVector): Boolean = dot(a, b) == 0.0

    def isOrthogonal(a1: Point, a2: Point, b1: Point, b2: Point): Boolean = {
      isOrthogonal(a1 - a2, b1 - b2)
    }

    def isOrthogonal(s1: Segment, s2: Segment): Boolean = {
      dot(s1.p2 - s1.p1, s2.p2 - s2.p1) == 0.0
    }

    // 外積
    def cross(a: Point, b: Point): Double = a.x * b.y - a.y * b.x

    def isParallel(a: GeoVector, b: GeoVector): Boolean = cross(a, b) == 0.0

    def isParallel(a1: Point, a2: Point, b1: Point, b2: Point): Boolean = {
      isParallel(a1 - a2, b1 - b2)
    }

    def isParallel(s1: Segment, s2: Segment): Boolean = {
      cross(s1.p2 - s1.p1, s2.p2 - s2.p1) == 0.0
    }

  }


  def main(args: Array[String]): Unit = {

    val n = StdIn.readLine().toInt
    val list = (0 until n).map { i =>
      val a = StdIn.readLine().split(' ').map(_.toDouble)
      (Point(a(0), a(1)), Point(a(2), a(3)), Point(a(4), a(5)), Point(a(6), a(7)))
    }

    list.foreach { ls =>
      if (Cgl0A.isParallel(ls._1, ls._2, ls._3, ls._4)) {
        println("2")
      } else if (Cgl0A.isOrthogonal(ls._1, ls._2, ls._3, ls._4)) {
        println("1")
      } else {
        println("0")
      }
    }
  }

}

