package Cgl

import Cgl.Cgl7D.Point


object Cgl0A {

  case class Point(x: Double = 0, y: Double = 0) {

    def +(p: Point): Point = Point(this.x + p.x, this.y + p.y)

    def -(p: Point): Point = Point(this.x - p.x, this.y - p.y)

    def *(a: Double): Point = Point(this.x * a, this.y * a)

    def /(a: Double): Point = {
      assert(a != 0)
      Point(this.x / a, this.y / a)
    }

    def norm(): Double = this.x * this.x + this.y * this.y

    def abs(): Double = scala.math.sqrt(norm())

    def <(p: Point): Boolean = this.x < p.x && this.y < p.y



    def ==(p: Point): Boolean = scala.math.abs(this.x - p.x) < Cgl0A.EPS &&
      scala.math.abs(this.y - p.y) < Cgl0A.EPS
  }

  case class Circle(center: Point, r: Int)

  case class Segment(p1: Point, p2: Point)

  case class Line(p1: Point, p2: Point)
  
  type GeoVector = Point

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
    else getDistanceLP(Line(s.p1, s.p2), a)
  }

  def getDistance(s1: Segment, s2: Segment): Double = {
    if (intersect(s1, s2)) {
      0.0
    }
    else {
      math.min(math.min(getDistanceSP(s1, s2.p1), getDistanceSP(s1, s2.p2)),
        math.min(getDistanceSP(s2, s1.p1), getDistanceSP(s2, s1.p2)))
    }
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
    else if (cross(a, b) < -1 * EPS) CLOCKWISE
    else if (dot(a, b) < -1 * EPS) ONLINE_BACK
    else if (a.norm() < b.norm()) ONLINE_FRONT
    else ON_SEGMENT

  }

  def intersect(p1: Point, p2: Point, p3: Point, p4: Point): Boolean = {
    Cgl0A.ccw(p1, p2, p3) * Cgl0A.ccw(p1, p2, p4) <= 0 &&
      Cgl0A.ccw(p3, p4, p1) * Cgl0A.ccw(p3, p4, p2) <= 0
  }

  def intersect(s1: Segment, s2: Segment): Boolean = {
    Cgl0A.intersect(s1.p1, s1.p2, s2.p1, s2.p2)
  }

  def getCrossPoint(s1: Segment, s2: Segment): Point = {
    val base: GeoVector = s2.p2 - s2.p1
    val d1 = scala.math.abs(cross(base, s1.p1 - s2.p2))
    val d2 = scala.math.abs(cross(base, s1.p2 - s2.p1))
    val t = d1 / (d1 + d2)
    s1.p1 + (s1.p2 - s1.p1) * t
  }


  def getCrossPoint(c: Circle, l: Line): Array[Point] = {
    val pr: GeoVector = project(Segment(l.p1, l.p2), c.center)
    val e: GeoVector = (l.p2 - l.p1) / (l.p2 - l.p1).abs()
    val base: Double = scala.math.sqrt(c.r*c.r - (pr - c.center).norm())
    Array(pr + e*base, pr - e * base).sortBy(p => (p.x, p.y))
  }

  // arq tangent
  def arg(p: GeoVector): Double = scala.math.atan2(p.y, p.x)

  // 
  def polar(a: Double, r: Double): GeoVector = {
    // x * 角度ラジアン
    Point(scala.math.cos(r) * a, scala.math.sin(r) * a)
  }

  def getCrossPoint(c1: Circle, c2: Circle): Array[Point] = {
    val d: Double = (c1.center - c2.center).abs()
    val cos: Double = (c1.r * c1.r + d * d - c2.r * c2.r) / (2 * c1.r * d)
    val a: Double = scala.math.acos(cos) // radian

    val t: Double = arg(c2.center - c1.center) // radian
    Array(c1.center + polar(c1.r, t + a),
      c1.center + polar(c1.r, t - a)).sortBy(p => (p.x, p.y))

  }

  val IN: Int = 2
  val ON: Int = 1
  val OUT: Int = 0

  def contains(g: IndexedSeq[Point], target: Point): Int = {
    val n = g.size
    var i = 0
    var onJudge = false
    var parity = false
    while(!onJudge && i < n){
      var a: Point = g(i) - target
      var b: Point = g((i+1)%n) - target
      if(cross(a, b).abs < EPS && dot(a, b) < EPS){
        onJudge = true
      } else {
        if(a.y > b.y){
          val temp = a
          a = b
          b = temp
        }
        parity = if(a.y < EPS && EPS < b.y && cross(a, b) > EPS) !parity else parity
      }
      i += 1
    }

    if(onJudge) {
      ON
    } else {
      if(parity) IN else OUT
    }
  }

}