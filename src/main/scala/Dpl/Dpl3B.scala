package Dpl

import scala.io.StdIn

object Dpl3B extends App {

  val Array(h, w) = StdIn.readLine().split(' ').map(_.toInt)

  val tiles = (0 until h).map(_ => StdIn.readLine().split(' ')).toArray
  val square = Array.fill[Int](h, w)(1)

  (0 until h).foreach { i =>
    (0 until w).foreach { k =>
      square(i)(k) = if (tiles(i)(k) == "1") {
        0
      } else if (i == 0) {
        square(i)(k)
      } else {
        square(i - 1)(k) + 1
      }
    }
  }

  case class Rect(height: Int, pos: Int)

  def pop(lst: List[Rect], row: Array[Int], index: Int): (List[Rect], Int, Int) = {
    val currentH = if (index == row.length) row(index - 1) else row(index)
    lst.foldLeft((List.empty[Rect], 0, 0)) { (acc, e) =>
      if (e.height >= currentH) {
        (acc._1, scala.math.max(acc._2, e.height * (index - e.pos)), e.pos)
      } else {
        (e :: acc._1, acc._2, acc._3)
      }
    }
  }

  def loop(lst: List[Rect], row: Array[Int], index: Int, max: Int): Int = {
    if (index == row.length) {
      scala.math.max(max, pop(lst, row, index)._2)
    } else if (lst.isEmpty || lst.head.height < row(index)) {
      loop(Rect(row(index), index) :: lst, row, index + 1, max)
    } else if (lst.head.height == row(index)) {
      loop(lst, row, index + 1, max)
    } else {
      val r = pop(lst, row, index)
      if (r._1.isEmpty) {
        scala.math.max(max, r._2)
      }
      else {
        loop(Rect(row(index), r._3) :: r._1, row, index + 1, max)
      }
    }
  }

  val result = square.foldLeft(0L) { (acc, elm) =>
    scala.math.max(acc, loop(List.empty[Rect], elm, 0, 0))
  }

  println(result)
}
