package Dpl

import scala.io.StdIn

object Dpl3A extends App {

  val Array(h, w) = StdIn.readLine().split(' ').map(_.toInt)

  val tiles = (0 until h).map(_ => StdIn.readLine().split(' ')).toArray
  val square = Array.fill[Int](h + 1, w + 1)(0)
  var max = 0

  (1 to h).foreach { i =>
    (1 to w).foreach { k =>
      square(i)(k) = if (tiles(i - 1)(k - 1) == "0") {
        scala.math.min(square(i)(k - 1),
          scala.math.min(square(i - 1)(k - 1), square(i - 1)(k))) + 1
      } else {
        0
      }

      max = scala.math.max(square(i)(k), max)
    }
  }

  println(max * max)

}

/*
  (1 to h).foreach { i =>
    (1 to w).foreach { k =>
      square(i)(k) = if(tiles(i-1)(k-1) == "0"){
        val t1 = square(i)(k-1)
        val t2 = square(i-1)(k-1)
        val t3 = square(i-1)(k)

        (if(t1 < t2){
          if(t1 < t3) t1 else t3
        } else if(t2 < t3){
          t2
        } else {
          t3
        }) + 1
      } else {
        0
      }

      max = scala.math.max(square(i)(k), max)
    } 
  }
 */

/*
4 5
0 0 1 0 0
1 0 0 0 0
0 0 0 1 0
0 0 0 1 0
-> 4

4 5
0 0 1 0 0
1 0 0 0 0
0 0 0 0 0
0 0 0 0 1

 */
