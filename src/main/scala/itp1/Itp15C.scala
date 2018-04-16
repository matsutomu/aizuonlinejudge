package itp1

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp15C extends App {

  var temp = Array(-1, -1)
  val buff = new ListBuffer[(Int, Int)]
  while(temp(0) != 0 && temp(1) != 0){
    temp = StdIn.readLine().split(' ').map(_.toInt)
    buff += ((temp(0), temp(1)))
  }

  val even = "."
  val odd  = "#"
  
  val result = buff.dropRight(1).map { case (h, w) =>
    (0 until h).map { j =>
      (0 until w).map(k => if((j+k)%2==0) "#" else ".").mkString("")
    }.mkString(f"%n")
  }
  println(result.mkString(f"%n%n"))
  println("")
}

/*
 too long
      if(j % 2 == 0){
        (0 until w).map(k => if(k%2==0) "#" else ".").mkString("")
      } else {
        (0 until w).map(k => if(k%2==0) "." else "#").mkString("")
      }
*/