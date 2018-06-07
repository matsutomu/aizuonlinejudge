package Ntl

object Ntl1A {

  def isPrime(x: Int): Boolean = {
    if (x == 2) {
      true
    }
    else if (x < 2 || x % 2 == 0) {
      false
    } else {
      !(3 to scala.math.sqrt(x).toInt by 2).exists(i => x % i == 0)
    }
  }


  
  
  
}
