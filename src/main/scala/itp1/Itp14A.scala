package itp1

import scala.io.StdIn

object Itp14A extends App {
  
  val a = StdIn.readLine().trim.split(' ').map(_.toInt)
  val d: Int = a(0)/a(1)
  val r: Int = a(0)%a(1)
  val f: Double = a(0).toDouble/a(1).toDouble
  
  print(d + " " + r + " " + f"$f%.5f")

}
