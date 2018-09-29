package Alds

import scala.io.StdIn

object Alds113A extends App {

  val matrixSize = 8
  val free = -1
  val notFree = 1

  val rows = Array.fill(matrixSize)(free)
  val cols = Array.fill(matrixSize)(free)
  val dpos = Array.fill(2 * matrixSize - 1)(free)
  val dneg = Array.fill(2 * matrixSize - 1)(free)

  val x = Array.fill(8, 8)(false)


  val n = StdIn.readLine().toInt
  (0 until n).foreach { _ =>
    val Array(r, c) = StdIn.readLine().split(" ").map(_.toInt)
    x(r)(c) = true
  }

  def printBoard(): Unit = {
    val rc = for (r <- 0 until matrixSize;
                  c <- 0 until matrixSize if x(r)(c)
    ) yield (r, c)
    val judge = rc.forall(e => rows(e._1) == e._2)
    if (judge) {
      for (r <- 0 until matrixSize;
           c <- 0 until matrixSize) {
        print(if (rows(r) == c) "Q" else ".")
        if (c == matrixSize - 1) println()
      }
    }
  }

  def recursive(i: Int): Unit = {
    if (i == matrixSize) {
      printBoard()
    } else {
      for (j <- 0 until matrixSize) {
        if (cols(j) != notFree
          && dpos(i + j) != notFree
          && dneg(i - j + matrixSize - 1) != notFree) {
          rows(i) = j
          cols(j) = notFree
          dpos(i + j) = notFree
          dneg(i - j + matrixSize - 1) = notFree
          recursive(i + 1)
          rows(i) = free
          cols(j) = free
          dpos(i + j) = free
          dneg(i - j + matrixSize - 1) = free

        }
      }
    }
  }

  // main start
  recursive(0)

}

