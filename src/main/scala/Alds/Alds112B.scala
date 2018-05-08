package Alds

import scala.io.StdIn

object Alds112B extends App {

  val n = StdIn.readLine().toInt
  val M = Array.fill[Int](n, n)(Int.MaxValue)

  (0 until n).foreach { _ =>
    val a = StdIn.readLine().split(' ').map(_.toInt)
    (0 until a(1)).foreach { k =>
      M(a(0))(a(2 + k * 2)) = a(2 + k * 2 + 1)
    }
  }

  // (0 until n).foreach { i => println(M(i).mkString("")) }
  private val WHITE = 0
  private val GRAY = 1
  private val BLACK = 9

  val color = Array.fill[Int](n)(WHITE)
  val distance = Array.fill(n)(Int.MaxValue)
  val parent = Array.fill(n)(-1)

  def dijkstra(s: Int): Unit = {
    var mincost = -1
    var u = 0
    distance(s) = 0
    parent(s) = -1

    while (mincost != Int.MaxValue) {
      mincost = Int.MaxValue
      (0 until n).foreach { i =>
        if (color(i) != BLACK && distance(i) < mincost) {
          mincost = distance(i)
          u = i
        }
      }

      if (mincost != Int.MaxValue) {
        color(u) = BLACK

        (0 until n).foreach { v =>
          if (color(v) != BLACK && M(u)(v) != Int.MaxValue) {
            if (distance(u) + M(u)(v) < distance(v)) {
              distance(v) = distance(u) + M(u)(v)
              parent(v) = u
              color(v) = GRAY
            }
          }
        }

      }

    }
  }

  dijkstra(0)

  (0 until n).foreach(i => println(i + " " + distance(i)))

}

