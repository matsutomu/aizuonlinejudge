package Alds

import scala.io.StdIn
object Alds11d extends App {

  val n = StdIn.readLine().trim.toInt

  var minv = StdIn.readLine().trim.toInt
  var maxv = (scala.math.pow(10, 9) * -1 ).toInt
  (1 until n).foreach{ j =>
    val temp = StdIn.readLine().trim.toInt
    maxv = if(maxv < temp - minv) temp - minv else maxv
    minv = if(temp < minv) temp else minv
  }

  println(maxv)
  
}
