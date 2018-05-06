package Alds

import scala.io.StdIn

object Alds111B extends App {

  val n = StdIn.readLine().toInt
  val result = Array.ofDim[Int](n, n)
  (0 until n).foreach { _ =>
    val a = StdIn.readLine().split(' ').map(_.toInt)
    (0 until a(1)).foreach { k =>
      result(a(0) - 1)(a(2 + k) - 1) = 1
    }
  }
  
  //(0 until n).foreach(i => println((i+1) + ": " +result(i).mkString(" ")))
  val edges = Array.ofDim[Int](n) // 0: ini, 1: current, 9: finish
  val timeS = Array.ofDim[Int](n)
  val timeE = Array.ofDim[Int](n)
  var time = 0

  private val WHITE = 0
  private val GRAY = 1
  private val BLACK = 9

  def depthFirstSearch(u: Int): Unit = {
    edges(u) = GRAY
    time += 1
    timeS(u) = time
    (0 until n).foreach { v =>
      if(result(u)(v) == 1 && edges(v) == WHITE){
        depthFirstSearch(v)
      }
    }
    edges(u) = BLACK
    time += 1
    timeE(u) = time
  }

  (0 until n).foreach( v => if(edges(v) == WHITE)depthFirstSearch(v))
  val r = (0 until n).map(v => s"${v+1} ${timeS(v)} ${timeE(v)}")
  
  println(r.mkString(f"%n"))
  
}

