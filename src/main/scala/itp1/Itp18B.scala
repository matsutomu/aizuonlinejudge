package itp1

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp18B extends App {

  var target = "-1"
  val buff = ListBuffer.empty[String]
  while(target != "0"){
    target = StdIn.readLine()
    /*buff += target.foldLeft(0){ (acc, f) =>
      acc + f.toString.toInt
    }.toString*/
    buff += target.map(_.toString.toInt).sum.toString
  }
  
  println(buff.dropRight(1).mkString(f"%n"))
}

