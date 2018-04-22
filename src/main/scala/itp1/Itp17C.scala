package itp1

import scala.io.StdIn

object Itp17C extends App {

  val Array(r, c) = StdIn.readLine().split(' ').map(_.toInt)
  val result = Array.fill(r, c+1)(0) 
  val temp = Array.fill(c+1)(0)
  
  (0 until r).foreach{ i =>
    result(i) = (StdIn.readLine() + " 0").split(' ').map(_.toInt)
    result(i)(c) = result(i).sum

    (0 to c).foreach{ k =>
      temp(k) += result(i)(k)
    }
  }

  (0 until r).foreach{ i =>
    println(result(i).toList.mkString(" "))
  }
  println(temp.toList.mkString(" "))
  
}

