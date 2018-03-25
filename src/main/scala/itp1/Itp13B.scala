package itp1

import scala.io.StdIn
import scala.collection.mutable.ListBuffer

object Itp13B extends App {
  var n = Int.MaxValue
  var i = 1
  val buff = ListBuffer.empty[(Int, Int)]
  while(n != 0){
    n = StdIn.readLine().trim.toInt
    if(n != 0) {
      buff += ((i, n))
    }
    i += 1
  }
  buff.foreach{ p =>
    println(s"Case ${p._1}: ${p._2}")
  }
}
