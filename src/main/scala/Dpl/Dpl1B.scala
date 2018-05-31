package Dpl

import scala.io.StdIn

object Dpl1B extends App {

  val Array(n, w) = StdIn.readLine().split(' ').map(_.toInt)

  case class Item(index: Int, value: Int, weight: Int)

  val items = (1 to n).map { i =>
    val a = StdIn.readLine().split(' ').map(_.toInt)
    Item(i, a(0), a(1))
  }

  val c = Array.fill[Int](n + 1, w + 1)(0)
  val g = Array.fill[Int](n + 1, w + 1)(1)

  val DIAGONAL = 1
  val TOP = 0

  items.foreach { i =>
    (1 to w).foreach { k =>
      if (i.weight <= k) {
        if (c(i.index - 1)(k - i.weight) + i.value > c(i.index - 1)(k)) {
          c(i.index)(k) = c(i.index - 1)(k - i.weight) + i.value
          g(i.index)(k) = DIAGONAL
        } else {
          c(i.index)(k) = c(i.index - 1)(k)
          g(i.index)(k) = TOP
        }
      } else {
        c(i.index)(k) = c(i.index - 1)(k)
        g(i.index)(k) = TOP
      }
    }

  }

  println(c(n)(w))

}


