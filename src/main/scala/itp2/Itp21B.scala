package itp2

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Itp21B extends App {
  
  // runtime error 
  val a = ArrayBuffer.empty[Int]
  def pushTop(x: Int): Unit = a.insert(0, x)
  def pushLast(x: Int): Unit = a.append(x)
  def popTop(): Unit = if(a.nonEmpty) a.remove(0)
  def popLast(): Unit = if(a.nonEmpty) a.remove(a.size - 1)
  def randomAccess(i: Int): Int = a(i)
  
  val q = StdIn.readLine().trim.toInt
  (0 until q).foreach { _ =>
    val input = StdIn.readLine()
    val cmds = input.split(' ').map(_.toInt)
    cmds match {
      case Array(0, d, x) if d == 0 =>  pushTop(x)
      case Array(0, d, x) if d == 1 =>  pushLast(x)
      case Array(1, p)  => println(randomAccess(p))
      case Array(2, d) if d == 0 => popTop()
      case Array(2, d) if d == 1 => popLast()
    }
  }
}
