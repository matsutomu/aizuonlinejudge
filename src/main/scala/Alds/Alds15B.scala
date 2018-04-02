package Alds

import scala.io.StdIn

object Alds15B extends App {

  final val infity = Int.MaxValue
  var compareCount = 0
  
  @inline def copyToArray(start: Int, end: Int): Array[Int] = {
    val alsize = end - start + 1
    val ar = new Array[Int](alsize)
    Array.copy(a, start, ar, 0, alsize - 1)
    ar(alsize-1) = infity
    ar
  }
  
  def merge(left: Int, mid: Int, right: Int): Unit ={
    //var al = a.slice(left, mid) ++ Array(infity)
    val al = copyToArray(left, mid)
    
    //var ar = a.slice(mid, right) ++ Array(infity)
    val ar = copyToArray(mid, right)
    var i = 0
    var j = 0
    //println("left:" + al.toList)
    //println("right:" + ar.toList)
    (left until right).foreach{ k =>
      compareCount += 1
      if(al(i) <= ar(j)){
        a(k) = al(i)
        i += 1
      } else {
        a(k) = ar(j)
        j += 1
      } 
    }
    //println("all:" + a.toList.mkString(" "))
  }
  
  val n = StdIn.readLine().trim.toInt
  val a = StdIn.readLine().trim.split(' ').map(_.toInt)

  def mergeSort(left: Int, right: Int): Unit ={
    if(left+1 < right){
      val mid = (left + right) / 2
      mergeSort(left, mid)
      mergeSort(mid, right)
      merge(left, mid, right)
    }
  }
  
  mergeSort(0, a.length)
  println(a.toList.mkString(" "))
  println(compareCount)

}

