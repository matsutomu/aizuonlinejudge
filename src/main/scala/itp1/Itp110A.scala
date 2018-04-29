package itp1

import scala.io.StdIn

object Itp110A extends App {

  var Array(x1,y1,x2,y2) = StdIn.readLine().split(' ').map(_.toDouble)
  val result = scala.math.sqrt(scala.math.pow(x1-x2, 2) + scala.math.pow(y1-y2,2))
  
  println(f"$result%.8f")
  
}
