package Alds

import scala.io.StdIn

object Alds11B extends App {

  val ra = StdIn.readLine().trim.split(' ').map(_.toInt)

  def gcd(x: Int, y: Int): Int = {
    if(x % y == 0) y
    else gcd(y, x % y)
  }

  println(gcd(ra(0), ra(1)))
  
}
