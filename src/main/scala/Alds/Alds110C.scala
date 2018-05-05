package Alds

import scala.io.StdIn

object Alds110C extends App {

  private val N = 1000

  def lcs(x: String, y: String): Int = {
    val a = Array.fill(N+1, N+1)(0)
    val m = x.length
    val n = y.length
    var maxl = 0
    var tempX = " " + x
    var tempY = " " + y

    (1 to m).foreach{ i =>
      (1 to n).foreach{ j =>
        if(tempX(i) == tempY(j)){
          a(i)(j) = a(i-1)(j-1) + 1
        } else {
          a(i)(j) = scala.math.max(a(i-1)(j), a(i)(j-1))
        }
        maxl = scala.math.max(maxl, a(i)(j))
      }
    }
    maxl
  }

  val cnt = StdIn.readLine().toInt
  val result = (0 until cnt).map{ _ =>
    lcs(StdIn.readLine(), StdIn.readLine())
  }

  result.foreach(println)
}

/*
  def fib(k: Int): Int = {
    if(a(k) != 0){
      a(k)
    } else {
      a(k) = fib(k-1) + fib(k-2)
      a(k)
    }
  }
  
  // too late
  def fib(k: Int): Int = {
    if(k == 0 || k == 1) {
      1
    } else {
      fib(k-1) + fib(k-2)
    }
  }
 */
