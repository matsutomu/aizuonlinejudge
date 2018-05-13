package Alds

import scala.io.StdIn

object Alds110A extends App {

  val n = StdIn.readLine().toInt
  private val a: Array[Int] = Array.ofDim(45)
  a(0) = 1
  a(1) = 1

  (2 to n).foreach { i =>
    a(i) = a(i - 1) + a(i - 2)
  }

  println(a(n))
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
