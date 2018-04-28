package itp1

import scala.io.StdIn

object Itp19C extends App {

  val n = StdIn.readLine().toInt
  
  val result = (0 until n).foldLeft((0, 0)){ (acc, i) =>
    val Array(t, h) = StdIn.readLine().split(' ')
    
    if(t.compare(h) > 0) (acc._1 + 3, acc._2)
    else if(t.compare(h) < 0) (acc._1, acc._2 + 3)
    else (acc._1 + 1, acc._2 + 1)
  }
  
  println(result._1 + " " + result._2)
}
