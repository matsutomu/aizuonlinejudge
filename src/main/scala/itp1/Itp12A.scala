package itp1

import scala.io.StdIn

object Itp12A extends App {
  val Array(a, b) = StdIn.readLine().trim.split(' ').map(_.toInt)

  println{ 
    if(a < b) "a < b" 
    else if(a > b) "a > b" 
    else "a == b"
  }
  
}
