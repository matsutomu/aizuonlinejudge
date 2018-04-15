package itp1

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp15A extends App {

  // presentation error !!
  var temp = Array(-1, -1)
  val buff = new ListBuffer[(Int, Int)]
  while(temp(0) != 0 && temp(1) != 0){
    temp = StdIn.readLine().split(' ').map(_.toInt)
    buff += ((temp(0), temp(1)))
  }

  val result = buff.dropRight(1).map { case (h, w) =>
    (0 until h).map { _ =>
      Array.fill(w)("#").mkString("")
    }.mkString(f"%n")
  }
  print(result.mkString(f"%n") + f"%n")
  println("")
}

/*
// too late
var hw = StdIn.readLine().split(' ').map(_.toInt)
while(hw(0) != 0 && hw(1) != 0){
  (0 until hw(0)).foreach{ _ =>
  (0 until hw(1)).foreach( _ => print("#"))
  println("")
}
  println("")
  hw = StdIn.readLine().split(' ').map(_.toInt)
}
*/