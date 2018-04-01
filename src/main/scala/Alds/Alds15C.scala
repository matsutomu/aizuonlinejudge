package Alds

import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

/*
 ret
 http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=2232735#1
 */
object Alds15C extends App {
  class Point(val x: Double, val y: Double){
    override def toString: String = {
      f"$x%.8f $y%.8f"
    }
  }
  
  final val start = new Point(0, 0)
  final val end   = new Point(100, 0)
  final val th = scala.math.Pi * 60.0 / 180.0
  final val cos = math.cos(th)
  final val sin = math.sin(th)


  @inline def calcuPoint(p1: Point, p2: Point, a: Double, b: Double) = new Point((a*p1.x+b*p2.x)/3.0, (a*p1.y+b*p2.y)/3.0)
  
  
  def koch(d: Double, p1: Point, p2: Point, ret: ArrayBuffer[Point]): Unit = {
    if(d != 0){
      val s = calcuPoint(p1, p2, 2.0, 1.0)
      val t = calcuPoint(p1, p2, 1.0, 2.0)
      val tsx = t.x - s.x
      val tsy = t.y - s.y
      val u = new Point(tsx*cos - tsy*sin + s.x,
                        tsx*sin + tsy*cos + s.y)
      
      koch(d-1, p1, s, ret)
      // println(s)
      ret += s
      
      koch(d-1, s, u, ret)
      //println(u)
      ret += u
      
      koch(d-1, u, t, ret)
      // println(t)
      ret += t

      koch(d-1, t, p2, ret)

    }
  }
  
  var result = ArrayBuffer.empty[Point]
  val n = StdIn.readLine().trim.toInt

  //println(start)
  result += start
  koch(n, start, end, result)
  result += end

  //result.foreach(println)
  println(result.mkString(f"%n"))
  
  
}

/*
  val n = StdIn.readLine().trim.toInt
  class Point(val x: Double, val y: Double){
    override def toString: String = {
      f"$x%.8f $y%.8f"
    }
  }
  
  final val start = new Point(0, 0)
  final val end   = new Point(100, 0)
  final val th = scala.math.Pi * 60.0 / 180.0

  def calcuPoint(p1: Point, p2: Point, a: Double, b: Double) = new Point((a*p1.x+b*p2.x)/3.0, (a*p1.y+b*p2.y)/3.0)
  
  
  def koch(d: Double, p1: Point, p2: Point): Unit = {
    if(d != 0){
      val s = calcuPoint(p1, p2, 2.0, 1.0) 
      val t = calcuPoint(p1, p2, 1.0, 2.0)
      val u = new Point((t.x - s.x)*math.cos(th) - (t.y - s.y)*math.sin(th) + s.x,
                        (t.x - s.x)*math.sin(th) + (t.y - s.y)*math.cos(th) + s.y)
      koch(d-1, p1, s)
      // print s
      println(s)
      
      koch(d-1, s, u)
      // print u
      println(u)
      
      koch(d-1, u, t)
      // print t
      println(t)

      koch(d-1, t, p2)

    }
  }

  println(start)
  koch(n, start, end)
  println(end)
 */