package Alds

import scala.io.StdIn

object Alds110B extends App {

  private val n = StdIn.readLine().toInt
  val memo = Array.fill(n + 1, n + 1)(0)
  val p = Array.fill(n + 1)(0)

  (0 until n).foreach { i =>
    val Array(r, c) = StdIn.readLine().split(' ').map(_.toInt)
    p(i) = r
    p(i + 1) = c
  }
  //println(p.toList)

  def matrixChainMultiplication(): Unit = {
    (2 to n).foreach { l =>
      (1 to (n - l + 1)).foreach { i =>
        val j = i + l - 1
        memo(i)(j) = Int.MaxValue
        (i until j).foreach { k =>
          memo(i)(j) = scala.math.min(memo(i)(j), memo(i)(k) + memo(k + 1)(j) + p(i - 1) * p(k) * p(j))
        }
      }
    }
  }

  matrixChainMultiplication()

  println(memo(1)(n))

}

