package Alds

import scala.io.StdIn

object Alds14D extends App {

  val Array(n, k) = StdIn.readLine().trim.split(' ').map(_.toInt)
  val list = (0 until n).map{ _ => StdIn.readLine().trim.toLong}.toVector
    
  def judge(p: Long): Long = {
    var i = 0
    var j = 0
    while(j < k){
      var c = 0L
      while(i < n && c + list(i) <= p){
        c += list(i)
        i += 1
      }
      j = if(i == n) k else j + 1
    }
    i
  }

  var left: Long = list.max
  var right: Long = 100000 * 10000
  var mid: Long  = 0

  // 徐々に間を縮めていく感じで、最終が１（0で抜ける？）
  while(right - left > 1){
    mid = (left + right) / 2
    val v = judge(mid)
    if(v >= n){
      right = mid
    } else {
      left = mid
    }
  }

  println(right)

}

/*
//
// http://judge.u-aizu.ac.jp/onlinejudge/review.jsp?rid=2298030#1
//


val sc = new java.util.Scanner(System.in)
val n, k = sc.nextInt
val list = List.fill(n)(sc.nextLong)

//println(list.size)

def judge(i: Int, j: Int, c: Long, p: Long): Boolean = {
if(i == n) true
else if(j == k) false
else {
if(c + list(i) <= p) judge(i+1, j, c+list(i), p) else judge(i,j+1,0,p)
}
}

def scan(left:Long, right:Long):Long = {
if(left<right) {
val mid = (left+right)/2
if(judge(0,0,0,mid)) scan(left,mid)
else scan(mid+1, right)
} else right
}

val l = list.max
val r = 100000 * 10000
*/

/*
 runtime error

*/