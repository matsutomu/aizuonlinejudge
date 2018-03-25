package itp1

import scala.io.StdIn

object Itp11C extends App {
  val Array(w, h) = StdIn.readLine().trim.split(' ').map(_.toInt)
  println(w*h + " " + ((w+h)*2))
}
