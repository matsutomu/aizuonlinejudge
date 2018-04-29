package Alds

import scala.io.StdIn

object Alds19C extends App {

  val max = 20000000
  val a: Array[Long] = Array.ofDim[Long](max+1)
  var h: Long = 0

  def insert(key: Long): Unit = {
    h += 1
    a(h) = -1*Long.MaxValue
    heapIncreaseKey(a, h, key)
  }
  
  private def heapIncreaseKey(heap: Array[Long], i: Long, key: Long): Unit = {
    heap(i) = key
    var idx = i
    var temp: Long = heap(parent(i))
    while(idx > 1 && temp < a(i)){
      swap(heap, i, parent(i))
      idx = parent(i)
      temp = heap(parent(i))
    }
  }
  
  def parent(i: Long): Long = i/2
  def left(i: Long): Long   = 2*i
  def right(i: Long): Long  = 2*i+1
  
  def swap(target: Array[Long], from: Long, to: Long): Unit = {
    val temp = target(to)
    
    target(to) = target(from)
    target(from) = temp
  }

//  def maxHeapify(a: Array[Long], i: Long): Unit = {
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

