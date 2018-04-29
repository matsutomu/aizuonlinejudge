package Alds

import scala.io.StdIn

object Alds19B extends App {

  val n = StdIn.readLine().trim.toInt
  val a = Array(0) ++ StdIn.readLine().split(' ').map(_.toInt)

  def parent(i: Int) = i/2
  def left(i: Int)  = 2*i
  def right(i: Int) = 2*i+1
  
  def swap(target: Array[Int], from: Int, to: Int): Unit = {
    val temp = target(to)
    target(to) = target(from)
    target(from) = temp
  }

  def maxHeapify(a: Array[Int], i: Int): Unit = {
    val p = parent(i)
    val l = left(i)
    val r = right(i)

    var largest = if(l <= n && a(l) > a(i)) l else i
    largest = if(r <= n && a(r) > a(largest)) r else largest
    
    if(i != largest){
      swap(a, i, largest)
      maxHeapify(a, largest)
    }
  }
  
  (n/2 to 1 by -1).foreach{ i =>
    maxHeapify(a, i)
  }

  println(" " + a.drop(1).mkString(" "))
}

