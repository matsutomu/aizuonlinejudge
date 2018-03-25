package Alds

import scala.io.StdIn

object Alds12A extends App {

  val rn = StdIn.readLine().trim.toInt
  val ra = StdIn.readLine().trim.split(' ').map(_.toInt)

  def bubbleSort(a: Array[Int], n: Int): (Array[Int], Int) = {
    var flag = true
    var i = 0
    var changeCount = 0
    while(flag){
      flag = false
      //(rn - 1 to 1 by -1).foreach{ j =>
      (rn - 1 to i + 1 by -1).foreach{ j =>
        if(a(j) < a(j - 1)){
          val t = a(j)
          a(j) = a(j - 1)
          a(j - 1) = t
          flag = true
          changeCount += 1
        }
      }
      i += 1
    }
    (a, changeCount)
  }

  val (r, c) = bubbleSort(ra, rn)
  println(r.toList.mkString(" "))
  println(c)
  
}
