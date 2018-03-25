package Alds

import scala.io.StdIn

object Alds11A extends App {

  val rn = StdIn.readLine().trim.toInt
  val ra = StdIn.readLine().trim.split(' ').map(_.toInt)

  def insertionSort(a: Array[Int], n: Int): Array[Int] = {
    var v = 0
    var j = 0
    (1 until n).foreach { i =>
      println(a.toList.mkString(" "))
      v = a(i)
      j = i - 1
      while(j >= 0 && a(j) > v){
        a(j+1) = a(j)
        j -= 1
      }
      a(j+1) = v
    }
    a
  }

  val r = insertionSort(ra, rn)
  println(r.toList.mkString(" "))
  
}
