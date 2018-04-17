package itp1

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp15D extends App {

  val n = StdIn.readLine().toInt
  val buff = ListBuffer.empty[Int]
  (1 to n).foreach { i =>
    var x = i
    if(x % 3 == 0) {
      buff += x
    } else {
      var continue = true
      do {
        if(x % 10 == 3) {
          buff += i
          continue = false
        } else {
          x = x / 10
        }
      } while(x != 0 && continue)
    } 
  }
  println(" " + buff.mkString(" "))
}

/*
30
 3 6 9 12 13 15 18 21 23 24 27 30

60
 3 6 9 12 13 15 18 21 23 24 27 30 31 32 33 34 35 36 37 38 39 42 43 45 48 51 53 54 57 60

NG 60
 3 6 9 12 13 15 18 21 23 24 27 30 3 3 33 3 3 36 3 3 39 42 43 45 48 51 53 54 57 60

*/