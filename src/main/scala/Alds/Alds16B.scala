package Alds

import scala.io.StdIn

object Alds16B extends App {
  val n = StdIn.readLine().trim.toInt
  val a = StdIn.readLine().trim.split(' ').map(_.toInt)

  val last = a(n - 1)

  @inline private def swap(from: Int, to: Int): Unit = {
    //println(s"from: $from  to: $to")
    val temp = a(from)
    a(from) = a(to)
    a(to)   = temp
  }
  
  def partition(arr: Array[Int], p: Int, r: Int): Int = {
    var i = p
    (p until r).foreach{ j =>
      while(arr(j) <= last){
        swap(j, i)
        //println(a.toList.mkString(" "))
        i += 1
      }
    }
    //swap(r-1, i+1)
    //i+1
    i
  }
  
  val num = partition(a, 0, n)
  val a1 = new Array[Int](num - 1)
  Array.copy(a, 0, a1, 0, num - 1)
  print(a1.mkString(" "))
  print(s" [$last] ")
  val a2 = new Array[Int](n - num)
  Array.copy(a, num, a2, 0, n - num)
  println(a2.mkString(" "))
  
  
}

