package itp1

import scala.io.StdIn

object Itp11D extends App {
  val s = StdIn.readLine().trim.toInt
  val basemin   = 60
  val basehour  = 60 * 60

  val rs = s % 60
  var temp = s - rs
  val rh = temp / basehour

  temp = temp - basehour * rh

  val rm = temp / basemin

  println(s"$rh:$rm:$rs")
}
