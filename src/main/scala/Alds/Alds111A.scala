package Alds

import scala.io.StdIn

object Alds111A extends App {
  
  val n = StdIn.readLine().toInt
  val result = Array.ofDim[Int](n, n)
  val r = (0 until n).map { _ =>
    val a = StdIn.readLine().split(' ').map(_.toInt)
    (0 until a(1)).foreach { k =>
      result(a(0)-1)(a(2+k)-1) = 1
    }
    result(a(0)-1).mkString(" ")
  }

  println(r.mkString(f"%n"))

}

