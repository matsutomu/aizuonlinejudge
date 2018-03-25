package itp1

import scala.io.StdIn

object Itp12B extends App {
  val Array(a, b, c) = StdIn.readLine().trim.split(' ').map(_.toInt)

  println{ 
    if(a < b && b < c) "Yes" 
    else "No"
  }
  
}
