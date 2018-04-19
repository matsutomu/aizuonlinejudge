package itp1

import scala.io.StdIn

object Itp16D extends App {

  val Array(n, m) = StdIn.readLine().split(' ').map(_.toInt)
    
  val a = Array.fill(n,m)(0)
  
  (0 until n).foreach{ i =>
    a(i) = StdIn.readLine().split(' ').map(_.toInt)
  }

  val b = Array.fill(m)(0)
  (0 until m).foreach{ i =>
    b(i) = StdIn.readLine().toInt
  }

  val result = (0 until n).map { k =>
    (0 until m).foldLeft(0) { (acc, p) =>
       acc + a(k)(p)*b(p)
    }
  }
  
  //println(result.mkString(f"%n"))
  result.foreach(println)
  
}

