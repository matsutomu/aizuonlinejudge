package itp1

import scala.io.StdIn

object Itp12D extends App {
  val Array(w, h, x, y, r) = StdIn.readLine().trim.split(' ').map(_.toInt)

  
  println(if(x - r < 0 || x + r > w || y - r < 0 || y + r > h) "No" else "Yes")
  
}
