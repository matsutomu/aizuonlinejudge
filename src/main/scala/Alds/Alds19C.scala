package Alds

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Alds19C extends App {

  val max = 20000000
  val a: Array[Int] = Array.ofDim[Int](max+1)
  var h: Int = 0

  def insert(key: Int): Unit = {
    h += 1
    a(h) = Int.MinValue
    heapIncreaseKey(a, h, key)
    //println("ins :" + a.slice(0,h+1).toList.mkString(" "))

  }
  
  private def heapIncreaseKey(heap: Array[Int], i: Int, key: Int): Unit = {
    heap(i) = key
    var idx = i
    while(idx > 1 && heap(idx/2) < heap(idx)){
      swap(heap, idx, idx/2)
      idx = idx/2
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

  def maxHeapify(heap: Array[Int], i: Int): Unit = {
    //val n = heap.length
    val p = parent(i)
    val l = left(i)
    val r = right(i)

    var largest = if(l <= h && heap(l) > heap(i)) l else i
    largest = if(r <= h && heap(r) > heap(largest)) r else largest

    if(i != largest){
      swap(heap, i, largest)
      maxHeapify(heap, largest)
    }
  }

  def heapExtractMax(heap: Array[Int]): Int = {
    // println("ext :" + heap.slice(0,h+1).toList.mkString(" "))
    val max = heap(1)
    heap(1) = heap(h)
    h -= 1
    maxHeapify(heap, 1)
    max
  }

  val buff = ListBuffer.empty[Int]
  
  var cmd = ""
  while(cmd != "end"){
    cmd = StdIn.readLine()
    cmd.split(' ') match {
      case Array("insert", key) => insert(key.toInt)
      case Array("extract") => 
        buff += heapExtractMax(a)
      case _ =>
    }
  }

  buff.foreach(println)
}

/*
insert 8
insert 7
insert 2
extract
insert 19
insert 10
extract
extract
insert 8
extract
extract
insert 3
insert 4
insert 1
extract
extract
extract
end

 */
