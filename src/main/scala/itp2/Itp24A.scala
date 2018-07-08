package itp2

import scala.io.StdIn

object Itp24A extends App {

  val n = StdIn.readLine().toInt
  val a = StdIn.readLine().split(" ")
  val q = StdIn.readLine().toInt

  (0 until q).foreach { _ =>
    val qa = StdIn.readLine().split(" ").map(_.toInt)
    val buff = a.slice(qa(0), qa(1)).reverse
    buff.copyToArray(a, qa(0), qa(1))
    /*    
      buff.zipWithIndex.foreach{ case (s, index) =>
      a(qa(0) + index) = s
    }*/
  }
  println(a.mkString(" "))

}
