package itp1

import scala.io.StdIn

object Itp17D extends App {

  val Array(n, m, l) = StdIn.readLine().split(' ').map(_.toInt)
  //http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=2415390#1  
  //  val a = Array.fill(n, m)(0)
  //  val b = Array.fill(m, l)(0)
  //  val result = Array.fill(n, l)(0L)
  val a: Array[Array[Int]] = Array.ofDim[Int](n, m)
  val b: Array[Array[Int]] = Array.ofDim[Int](m, l)
  val result: Array[Array[Long]] = Array.ofDim[Long](n, l)
  
  (0 until n).foreach{ i =>
    a(i) = StdIn.readLine().split(' ').map(_.toInt)
  }
  (0 until m).foreach{ i =>
    b(i) = StdIn.readLine().split(' ').map(_.toInt)
  }

  val rout = (0 until n).map{ i =>
    (0 until l).foreach{ j =>
      result(i)(j) = (0 until m).foldLeft(0L){ (acc, k) =>
        acc + a(i)(k)*b(k)(j)
      }
    }
    result(i).mkString(" ")
  }.mkString(f"%n")

  println(rout)
}

/*
1 2
0 3
4 5

1 2 1
0 3 2

-> 
C11(A11*B11 + A12*B21) = (1*1 + 2*0), C12(1*2 + 2*3)

1 8 5
0 9 6
4 23 14

2409392806
2147483647
 */