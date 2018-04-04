package Alds

/*
3
3 1 2
-> 1 [2] 3

3
1 3 2
-> 1 [2] 3

12
13 19 9 5 12 8 7 4 21 2 6 11
-> 9 5 8 7 4 2 6 [11] 21 13 19 12

2
2 1
-> [1] 2

 */

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
    var i = p - 1
    (p until r).foreach{ j =>
      if(arr(j) <= last){
        i += 1
        swap(i, j)
        //println(a.toList.mkString(" "))
      }
    }
    swap(i+1, r)
    //println("last:" + a.toList.mkString(" "))
    i + 1
  }
  
  val partitionIndex = partition(a, 0, n-1)
  //println(s"num: $partitionIndex")
  
  println(a.slice(0, partitionIndex).mkString(" ") +
  " [" + a(partitionIndex) + "] " + 
    a.slice(partitionIndex + 1, n).mkString(" "))
  
}

/*
val a1 = new Array[Int](partitionIndex)
Array.copy(a, 0, a1, 0, partitionIndex)
print(a1.mkString(" ") + (if(a1.nonEmpty) " " else "" ))
print(s"[$last]")
val overIndex = partitionIndex + 1
val a2 = new Array[Int](n - (overIndex))
Array.copy(a, overIndex, a2, 0, n - (overIndex))
println( (if(a2.nonEmpty) " " else "" ) + a2.mkString(" "))
*/