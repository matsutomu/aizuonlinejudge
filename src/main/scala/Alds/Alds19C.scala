package Alds

import scala.io.StdIn

object Alds19C extends App {

  val max = 20000000
  val a: Array[Int] = Array.ofDim[Int](max+1)
  var h: Int = 0

  def insert(key: Int): Unit = {
    h += 1
    a(h) = Int.MinValue
    heapIncreaseKey(a, h, key)
  }
  
  private def heapIncreaseKey(heap: Array[Int], i: Int, key: Int): Unit = {
    heap(i) = key
    var idx = i
    var temp: Int = heap(parent(i))
    while(idx > 1 && temp < a(i)){
      swap(heap, i, parent(i))
      idx = parent(i)
      temp = heap(parent(i))
    }
  }
  
  def parent(i: Int): Int = i/2
  def left(i: Int): Int   = 2*i
  def right(i: Int): Int  = 2*i+1
  
  def swap(target: Array[Int], from: Int, to: Int): Unit = {
    val temp = target(to)
    
    target(to) = target(from)
    target(from) = temp
  }

//  def maxHeapify(a: Array[Int], i: Int): Unit = {
//    val n = a.length
//    val p = parent(i)
//    val l = left(i)
//    val r = right(i)
//
//    var largest = if(l <= n && a(l) > a(i)) l else i
//    largest = if(r <= n && a(r) > a(largest)) r else largest
//    
//    if(i != largest){
//      swap(a, i, largest)
//      maxHeapify(a, largest)
//    }
//  }
//  


  println(" " + a.drop(1).mkString(" "))
}

