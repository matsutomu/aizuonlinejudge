package Alds

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Alds111D extends App {

  val Array(n, m) = StdIn.readLine().split(' ').map(_.toInt)
  val groupIndex = Array.fill[Int](n)(-1)
  val friends = Array.fill[ArrayBuffer[Int]](n)(ArrayBuffer.empty[Int])

  (0 until m).foreach { _ =>
    val a = StdIn.readLine().split(' ').map(_.toInt)
    friends(a(0)) += a(1)
    friends(a(1)) += a(0)
  }

  def depthFirstSearch(u: Int, grpId: Int): Unit = {
    groupIndex(u) = if (groupIndex(u) == -1) grpId else groupIndex(u)
    friends(u).foreach { v =>
      if (groupIndex(v) == -1) {
        depthFirstSearch(v, grpId)
      }
    }
  }


  var idx = 1
  (0 until n).foreach { v =>
    if (groupIndex(v) == -1) {
      depthFirstSearch(v, idx + 1)
      idx += 1
    }
  }

  val q = StdIn.readLine().toInt
  //val sbbuff = new StringBuilder
  //println(s"q: $q")
  (0 until q).foreach { _ =>
    val ql = StdIn.readLine().split(' ').map(_.toInt)
    if (groupIndex(ql(0)) == groupIndex(ql(1))) {
      println("yes")
    } else {
      println("no")
    }
  }

  //print(sbbuff.toString())

}

/*
// late
def dps(r: Int, groupIdx: Int): Unit = {
  var buff = ArrayBuffer.empty[Int]
  buff += r
  groupIndex(r) = groupIdx
  while(buff.nonEmpty){
    val u = buff.head
    buff = buff.tail
    friends(u).foreach{ v =>
      if(groupIndex(v) == -1){
        groupIndex(v) = groupIdx
        buff += v
      }
    }
  }
}
*/
