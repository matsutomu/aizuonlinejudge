package Alds

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Alds111D extends App {

  val Array(n, m) = StdIn.readLine().split(' ').map(_.toInt)
  val groupIndex = Array.fill[Int](n)(-1)
  val friends = Array.ofDim[ArrayBuffer[Int]](n)
  
  (0 until m).foreach { _ =>
    val a = StdIn.readLine().split(' ').map(_.toInt)
    friends(a(0)) = if(friends(a(0)) == null) ArrayBuffer.empty[Int] else friends(a(0))
    friends(a(0)) += a(1)
    friends(a(1)) = if(friends(a(1)) == null) ArrayBuffer.empty[Int] else friends(a(1))
    friends(a(1)) += a(0)
  }

  def dps(r: Int, groupIdx: Int): Unit = {
    var buff = ArrayBuffer.empty[Int]
    buff += r
    groupIndex(r) = groupIdx
    while(buff.nonEmpty){
      val u = buff.head
      buff = buff.drop(1)
      friends(u).foreach{ v =>
        if(groupIndex(v) == -1){
          groupIndex(v) = groupIdx
          buff += v
        }
      }
    }
  }

  var idx = 1
  (0 until n).foreach { v =>
    if(groupIndex(v) == -1){
      dps(v, idx + 1)
      idx += 1
    }
  }

  val q = StdIn.readLine().toInt
  //println(s"q: $q")
  val r = (0 until q).map { _ =>
    val ql = StdIn.readLine().split(' ').map(_.toInt)
    if(groupIndex(ql(0)) == groupIndex(ql(1))){
      "yes"
    } else {
      "no"
    }
  }
  
  println(r.mkString(f"%n"))
  
}

