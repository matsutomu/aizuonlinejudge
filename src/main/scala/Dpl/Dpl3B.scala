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
    val currentH = if (index == row.length) 0 else row(index)
    val (heigher, least) = lst.partition(e => e.height >= currentH)
    val rh = heigher.foldLeft((0, 0)) { (acc, e) =>
      (scala.math.max(acc._1, e.height * (index - e.pos)), e.pos)
    }
    (least, rh._1, rh._2)
  }

  def loop(row: Array[Int]): Int = {
    (0 to w).foldLeft(List.empty[Rect], 0) { (acc, index) =>
      val (lst, temp) = acc
      if (lst.isEmpty) {
        (Rect(row(index), index) :: lst, temp)
      } else if (index == row.length) {
        val (l, max, pos) = pop(lst, row, index)
        (l, scala.math.max(max, temp))
      } else {
        if (lst.head.height < row(index)) {
          (Rect(row(index), index) :: lst, temp)
        } else if (lst.head.height > row(index)) {
          val (l, max, pos) = pop(lst, row, index)
          (Rect(row(index), pos) :: l, scala.math.max(max, temp))
        } else {
          (lst, temp)
        }
      }
    }._2

  }


  val result = square.foldLeft(0) { (acc, elm) =>
    //println(elm.mkString(" "))
    //println(s"$acc")
    scala.math.max(acc, loop(elm))
  }

  println(result)
}

/*
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
      loop(Rect(row(index), r._3) :: r._1.reverse, row, index + 1, max)
    }
  }
}*/