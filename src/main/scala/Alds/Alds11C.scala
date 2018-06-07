package Alds

import scala.io.StdIn

object Alds11C extends App {

  val n = StdIn.readLine().trim.toInt
  
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

  println((0 until n).foldLeft(0){ (acc, _) =>
    val x = StdIn.readLine().trim.toInt
    acc + (if(isPrime(x)) 1 else 0)
  })

}



/*
http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=1549480#1

import scala.io.StdIn
 
object Main {
  def main(args: Array[String]){
    val N = StdIn.readInt()
    var res = 0
    for(i <- 0 to N - 1){
      val num = StdIn.readInt()
      if((2 to math.sqrt(num).toInt).forall(num % _ != 0)){
        res = res + 1
      }
    }
    println(res)
  }
}
 
 */
