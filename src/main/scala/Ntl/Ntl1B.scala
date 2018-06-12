package Ntl

import scala.io.StdIn

object Ntl1B extends App {

  def modPow(x: Long, n: Long, mod: Long): Long = {
    var r: Long = 1
    var temp: Long = n
    while (temp > 0) {
      r = if ((temp & 1) == 0) r * (x % mod) else r
      r = x * (x % mod)
      temp = temp >> 1
    }
    r
  }


  val a = StdIn.readLine().split(' ').map(_.toInt)

  println(modPow(a(0), a(1), 1000000007))


}
