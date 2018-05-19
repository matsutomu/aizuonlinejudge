package Cgl

import scala.io.StdIn

object Cgl1B {

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


    def ==(p: Point): Boolean = scala.math.abs(this.x - p.x) < Cgl0A.EPS &&
      scala.math.abs(this.y - p.y) < Cgl0A.EPS


  }

  case class Segment(p1: Point, p2: Point)

  case class Line(p1: Point, p2: Point)

  type GeoVector = Point

  object Cgl0A {
    val EPS: Double = scala.math.pow(10,-10)

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

    def project(s: Segment, p: Point): Point = {
      val base: GeoVector = s.p2 - s.p1
      val r: Double = Cgl0A.dot(p - s.p1, base) / base.norm()
      s.p1 + base * r
    }

    def reflect(s: Segment, p: Point): Point = {
      p + (project(s, p) - p) * 2.0
    }

    def getDistance(a: Point, b: Point): Double = (a - b).abs()

    def getDistanceLP(l: Line, a: Point): Double = {
      scala.math.abs(cross(l.p2 - l.p1, a - l.p1) / (l.p2 - l.p1).abs())
    }

    def getDistanceSP(s: Segment, a: Point): Double = {
      if (dot(s.p2 - s.p1, a - s.p1) < 0.0) (a - s.p1).abs()
      else if (dot(s.p1 - s.p2, a - s.p2) < 0.0) (a - s.p2).abs()
      else getDistanceLP(s.asInstanceOf[Line], a)
    }

    val COUNTER_CLOCKWISE: Int = 1
    val CLOCKWISE: Int = -1
    val ONLINE_BACK: Int = 2
    val ONLINE_FRONT: Int = -2
    val ON_SEGMENT: Int = 0

    def ccw(p0: Point, p1: Point, p2: Point): Int = {
      val a: GeoVector = p1 - p0
      val b: GeoVector = p2 - p0

      if (cross(a, b) > EPS) COUNTER_CLOCKWISE
      else if (cross(a, b) < -1*EPS) CLOCKWISE
      else if (dot(a, b) < -1*EPS) ONLINE_BACK
      else if (a.norm() < b.norm()) ONLINE_FRONT
      else ON_SEGMENT
      
    }
    
  }


  def main(args: Array[String]): Unit = {
    val a = StdIn.readLine().split(' ').map(_.toDouble)
    val p1 = Point(a(0), a(1))
    val p2 = Point(a(2), a(3))
    val base = Segment(p1, p2)

    val q = StdIn.readLine().toInt
    (0 until q).foreach { i =>
      val ai = StdIn.readLine().split(' ').map(_.toDouble)
      val qi = Point(ai(0), ai(1))

      val r = Cgl0A.reflect(base, qi)
      println(f"${r.x}%.8f  ${r.y}%.8f")

    }

  }

}

