package Alds

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Alds111C extends App {

  val n = StdIn.readLine().toInt
  val result = Array.ofDim[Int](n, n)
  (0 until n).foreach { _ =>
    val a = StdIn.readLine().split(' ').map(_.toInt)
    (0 until a(1)).foreach { k =>
      result(a(0) - 1)(a(2 + k) - 1) = 1
    }
  }
  
  //(0 until n).foreach(i => println((i+1) + ": " +result(i).mkString(" ")))
  val color = Array.ofDim[Int](n) // 0: ini, 1: current, 9: finish
  val distance = Array.fill(n)(-1)
  var queue = ListBuffer.empty[Int]

  private val WHITE = 0
  private val GRAY = 1
  private val BLACK = 9

  color(0) = GRAY
  queue += 0 // start node
  distance(0) = 0
  while(queue.nonEmpty){
    val u = queue.head
    queue = queue.drop(1)
    (0 until n).foreach { v =>
      if(result(u)(v) == 1 && color(v) == WHITE){
        color(v) = GRAY
        distance(v) = distance(u) + 1
        queue += v
      }
    }
    color(u) = BLACK
  }
    

  val r = (0 until n).map(v => s"${v+1} ${distance(v)}")
  
  println(r.mkString(f"%n"))
  
}

