package itp1

import scala.io.StdIn

object Itp110D extends App {

  val n = StdIn.readLine().toInt
  val xs = StdIn.readLine().split(' ').map(_.toDouble)
  val ys = StdIn.readLine().split(' ').map(_.toDouble)
  
  def pat1(): Double = (xs zip ys).foldLeft(0D)((acc, xy) => acc + scala.math.abs(xy._1 - xy._2))

  def pat2(): Double = (xs zip ys).foldLeft(0D)((acc, xy) => acc + scala.math.pow(scala.math.abs(xy._1 - xy._2), 2))

  def pat3(): Double = (xs zip ys).foldLeft(0D)((acc, xy) => acc + scala.math.pow(scala.math.abs(xy._1 - xy._2), 3))

  def patL(): Double = (xs zip ys).foldLeft(0D)((acc, xy) => if(acc > scala.math.abs(xy._1 - xy._2)) acc else scala.math.abs(xy._1 - xy._2))

  val r1 = pat1()
  val r2 = scala.math.sqrt(pat2())
  val r3 = scala.math.cbrt(pat3())
  val rL = patL()

  println(f"$r1%.6f")
  println(f"$r2%.6f")
  println(f"$r3%.6f")
  println(f"$rL%.6f")
  
}

/*
  def pat1(): Double = (xs zip ys).foldLeft(0D){ (acc, xy) =>
      acc + scala.math.abs(xy._1 - xy._2)
  }

  def pat2(): Double = (xs zip ys).foldLeft(0D){ (acc, xy) =>
    acc + scala.math.pow(scala.math.abs(xy._1 - xy._2), 2)
  }

  def pat3(): Double = (xs zip ys).foldLeft(0D){ (acc, xy) =>
    acc + scala.math.pow(scala.math.abs(xy._1 - xy._2), 3)
  }

  def patL(): Double = (xs zip ys).foldLeft(0D){ (acc, xy) =>
    if(acc > scala.math.abs(xy._1 - xy._2)) acc else scala.math.abs(xy._1 - xy._2)
  }

  val r1 = pat1()
  val r2 = scala.math.sqrt(pat2())
  val r3 = scala.math.cbrt(pat3())
  val rL = patL()

  println(f"$r1%.6f")
  println(f"$r2%.6f")
  println(f"$r3%.6f")
  println(f"$rL%.6f")


 */