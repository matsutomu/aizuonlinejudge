package itp2

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Itp21A extends App {


  //val a = List.empty[Int]
  val a = ArrayBuffer.empty[Int]

  // 2,147,483,647
  def pushBuck(x: Int): Unit = a.append(x)

  def popBuck(): Unit = if(a.nonEmpty) a.remove(a.size - 1)

  def randomAccess(i: Int): Int = a(i)

  val q = StdIn.readLine().trim.toInt
  (0 until q).foreach { i =>
    val input = StdIn.readLine()
    val temp = if (input.startsWith("2")) Array(2, 0) else input.split(' ').map(_.toInt)
    val (cmd, x): (Int, Int) = (temp(0), temp(1))

    cmd match {
      case 0 =>
        pushBuck(x)
      case 1 => println(randomAccess(x))
      case 2 =>
        popBuck()
    }
  }
}

/*  val a = List.empty[Int]
  def pushBuck(x: Int, lst: List[Int]): List[Int] = x :: lst

  def popBuck(lst: List[Int]): List[Int] = lst.tail

  def randomAccess(i: Int, lst: List[Int]): Int = {
    val idx = if(lst.size <= i) 0 else lst.size - i - 1 
    lst(idx)
  } */
