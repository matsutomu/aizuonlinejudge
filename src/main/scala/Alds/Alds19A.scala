package Alds

import scala.io.StdIn

object Alds19A extends App {

  val n = StdIn.readLine().trim.toInt
  val a = Array(0) ++ StdIn.readLine().split(' ').map(_.toInt)

  def parent(i: Int) = i/2
  def left(i: Int)  = 2*i
  def right(i: Int) = 2*i+1

  def printNodeInfo(i: Int): Unit = {
    val p = parent(i)
    val l = left(i)
    val r = right(i)
    
    println(
    s"node $i:" +
    s" key = ${a(i)}, " + 
      (if(0 < p) s"parent key = ${a(p)}, " else "") +
      (if(l <= n) s"left key = ${a(l)}, " else "") +
      (if(r <= n) s"right key = ${a(r)}, " else "")
    )
  }

  (1 to n).foreach{ i =>
    printNodeInfo(i)
  }

}

