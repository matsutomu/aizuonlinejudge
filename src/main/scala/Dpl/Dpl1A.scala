package Dpl

import scala.io.StdIn

object Dpl1A extends App {


  val Array(n, m) = StdIn.readLine().split(' ').map(_.toInt)

  val t: Array[Int] = Array.fill[Int](n + 1)(Int.MaxValue)
  t(0) = 0

  val c = StdIn.readLine().split(' ').map(_.toInt)

  (0 until m).foreach { i =>
    for (j <- c(i) to n) {
      t(j) = scala.math.min(t(j), t(j - c(i)) + 1)
    }
  }

  println(t(n))

}


