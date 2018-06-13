package Ntl

import scala.io.StdIn

object Ntl1B extends App {

  def modPow(x: Long, n: Long, mod: Long): Long = {
    if(n == 0){
      1
    } else {
     val r = modPow(x * x % mod, n / 2, mod)
      if(n % 2 != 0){
        r * x % mod
      } else {
        r
      }
    }
  }

  val a = StdIn.readLine().split(' ').map(_.toInt)

  println(modPow(a(0), a(1), 1000000007))

}
