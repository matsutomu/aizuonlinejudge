package itp2

import scala.collection.mutable.ArrayBuffer
import scala.io.{Source, StdIn}

object Itp21B extends App {

  val source = Source.fromFile("/Users/matsutomu/work/scala_learn/aizuonlinejudge/src/test/scala/itp2_1_b_case10.txt")
  val lines = source.getLines
  
  
  // runtime error 
  val a = ArrayBuffer.empty[Int]

  // 2147483647
  // 631528937
  def pushTop(x: Int): Unit = a.insert(0, x)

  def pushLast(x: Int): Unit = a.append(x)

  def popTop(): Unit = if (a.nonEmpty) a.remove(0)

  def popLast(): Unit = if (a.nonEmpty) a.remove(a.size - 1)

  def randomAccess(i: Int): Int = a(i)

 // val q = StdIn.readLine().trim.toLong
 val q = lines.next().toInt
  var i = 0L
  while(i < q){
    //val input = StdIn.readLine()
    val input = lines.next()
    val cmds = input.split(' ').map(_.toInt)
    cmds match {
      case Array(0, d, x) if d == 0 => pushTop(x)
      case Array(0, d, x) if d == 1 => pushLast(x)
      case Array(1, p) => println(randomAccess(p))
      case Array(2, d) if d == 0 => popTop()
      case Array(2, d) if d == 1 => popLast()
    }
    i += 1
  }
}
