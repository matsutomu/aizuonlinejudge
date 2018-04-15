package itp1

import scala.io.StdIn

object Itp14B extends App {
  
  val r = StdIn.readLine().toDouble
  val s: Double = scala.math.Pi * r * r
  val v: Double = scala.math.Pi * r * 2
  
  print(f"$s%.6f" + " " + f"$v%.6f")

}
