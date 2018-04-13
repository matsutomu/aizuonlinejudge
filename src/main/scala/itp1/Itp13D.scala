package itp1

import scala.io.StdIn

object Itp13D extends App {
  
  val a = StdIn.readLine().trim.split(' ').map(_.toInt)
  val t = a(2)
  /*
  val result = (a(0) to a(1)).foldLeft(0) { (acc, i) =>
    if(t % i == 0) acc + 1 else acc
  }*/

  /*
   ref
   http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=2388634#1
   */
  //val result = (a(0) to a(1)).filter(p => t % p == 0).size
  // late 
  val result = (a(0) to a(1)).count(p => t % p == 0)
  println(result)

}
