package itp1

import scala.io.StdIn

object Itp110B extends App {

  val Array(a, b, c) = StdIn.readLine().split(' ').map(_.toDouble)
  val rad = scala.math.toRadians(c)
  val s = (scala.math.sin(rad) * a * b ) / 2
  val h = scala.math.sin(rad) * b
  // 余弦定理
  val x = scala.math.sqrt(a*a + b*b - 2*b*a*scala.math.cos(rad)) + a + b
  
  println(f"$s%.8f")
  println(f"$x%.8f")
  println(f"$h%.8f")
  
}

/*

OK)
100 80 54
3236.06797749979
263.6387228699747
64.7213595499958

3236.06797750
299.11697772
64.72135955

 */