package itp1

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp15B extends App {

  var temp = Array(-1, -1)
  val buff = new ListBuffer[(Int, Int)]
  while(temp(0) != 0 && temp(1) != 0){
    temp = StdIn.readLine().split(' ').map(_.toInt)
    buff += ((temp(0), temp(1)))
  }

  val result = buff.dropRight(1).map { case (h, w) =>
    (0 until h).map { j =>
      if(j == 0 || j == h - 1){
        Array.fill(w)("#").mkString("")
      } else {
        val temp = Array.fill(w)(".")
        temp(0) = "#";temp(w-1) = "#"
        temp.mkString("")
      }
    }.mkString(f"%n")
  }
  println(result.mkString(f"%n%n"))
  println("")
}

/*

*/