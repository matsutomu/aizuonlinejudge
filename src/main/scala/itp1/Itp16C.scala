package itp1

import scala.io.StdIn

object Itp16C extends App {

  val result = Array.fill(4,3,10)(0)
  
  val n = StdIn.readLine().toInt
  (0 until n).foreach{ i =>
    val Array(b, f, r, v) = StdIn.readLine().split(' ').map(_.toInt)
    result(b-1)(f-1)(r-1) += v
  }

  private final val border = "#"*20
  
  print(
    result.map { b =>
      b.map { r =>
        " " + r.mkString(" ") + f"%n"
      }.mkString("")
    }.mkString(border + f"%n")
  )
}

/*
3
1 1 3 8
3 2 2 7
4 3 8 1
0 0 8 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
####################
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
####################
0 0 0 0 0 0 0 0 0 0
0 7 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
####################
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 1 0 0
####################

 0 0 8 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
####################
 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
####################
 0 0 0 0 0 0 0 0 0 0
 0 7 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
####################
 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 1 0 0

 */