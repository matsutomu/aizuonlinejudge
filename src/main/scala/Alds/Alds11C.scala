package Alds

import scala.io.StdIn

object Alds11C extends App {

  val n = StdIn.readLine().trim.toInt
  val lst: Array[Int] = (0 until n).map{ _ =>
    StdIn.readLine().trim.toInt
  }.sorted.toArray
  
  var lsize = lst.size

  var evl = 2
  var max = lst.last
  while(evl < scala.math.sqrt(max)){
    (0 until lsize).foreach{ i =>
      val tmp = lst(i)
      if(tmp != evl){
        lst(i) = if(tmp % evl == 0) 0 else tmp
      }
    }
    evl += 1
  }

  println(lst.filter(p => p != 0).size)
  
}

/*
http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=1549480#1

import scala.io.StdIn
 
object Main {
  def main(args: Array[String]){
    val N = StdIn.readInt()
    var res = 0
    for(i <- 0 to N - 1){
      val num = StdIn.readInt()
      if((2 to math.sqrt(num).toInt).forall(num % _ != 0)){
        res = res + 1
      }
    }
    println(res)
  }
}
 
 */
