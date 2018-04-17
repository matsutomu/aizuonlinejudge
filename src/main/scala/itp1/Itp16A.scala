package itp1

import scala.io.StdIn

object Itp16A extends App {

  val n = StdIn.readLine().toInt
  var k = n - 1
  val out = Array.fill(n)("0")
  
  StdIn.readLine().split(' ').foreach{ e =>
    out(k) = e
    k -= 1
  }
  
  println(out.mkString(" "))
  
}

/*
reverse が普通に早い
*/

/*
  val n = StdIn.readLine().toInt
  val buff = StdIn.readLine().split(' ').map(_.toInt)
  val out = Array.fill(n)(0)
  @tailrec def printReverse(index: Int): Unit = {
    if(index >= 0) {
      out(n - (index + 1)) = buff(index)
      printReverse(index - 1)
    }
  }
  out.reverse
  printReverse(n-1)
  println(out.mkString(" "))
 */