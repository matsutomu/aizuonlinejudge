package Alds

import scala.io.StdIn

object Alds12B extends App {

  val rn = StdIn.readLine().trim.toInt
  val ra = StdIn.readLine().trim.split(' ').map(_.toInt)

  def selectionSort(a: Array[Int], n: Int): (Array[Int], Int) = {
    var changeCount = 0
    (0 until n).foreach{ i =>
      var minj = i
      (i until n).foreach{ j =>
        minj = if(a(j) < a(minj)) j else minj
      }
      val temp = a(minj)
      a(minj) = a(i)
      a(i) = temp
      changeCount = if(i != minj) changeCount + 1 else changeCount
    }
    (a, changeCount)
  }

  val (r, c) = selectionSort(ra, rn)
  println(r.toList.mkString(" "))
  println(c)
  
}
