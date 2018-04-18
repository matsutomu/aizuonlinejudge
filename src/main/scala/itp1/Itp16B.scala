package itp1

import scala.io.StdIn

object Itp16B extends App {

  val n = StdIn.readLine().toInt
  val emptyCards = Array.fill(13)(-1)
  val result = (0 until n).foldLeft(Map.empty[String, Array[Int]]){ (acc, _) =>
    val Array(inpict, innum) = StdIn.readLine().split(' ')
    val temp = acc.getOrElse(inpict, emptyCards.clone())
    temp(innum.toInt-1) = 1
    acc.updated(inpict, temp)
  }
  
  List("S","H","C","D").foreach{ pict =>
    val buff = result.getOrElse(pict, emptyCards.clone())
    (0 until 13).foreach{ index =>
      if(buff(index) == - 1){
        println(pict + " " + (index + 1))
      }
    }
  }
}

/*
reverse が普通に早い
*/

/*
  val n = StdIn.readLine().toInt
  val buff = StdIn.readLine().split(' ').map(_.toInt)
  val out = Array.fill(n)(0)
  @tailrec def printReverse(index: Int): Unit = {
    if(index >= 0) {
      out(n - (index + 1)) = buff(index)
      printReverse(index - 1)
    }
  }
  out.reverse
  printReverse(n-1)
  println(out.mkString(" "))
 */