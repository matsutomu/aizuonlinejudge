package Alds

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Alds12D extends App {

  val rn = StdIn.readLine().trim.toInt
  val ra = (0 until rn).map{ _ =>
    StdIn.readLine().trim.toInt
  }.toArray
  
  def insertionSort(a: Array[Int], n: Int, g: Int): (Array[Int], Int) = {
    var cnt = 0
    (g-1 until n).foreach{ i =>
      //println(s"insertion($i):" + a.toList)
      var v = a(i)
      var j = i - g
      while(j >= 0 && a(j) > v){
        a(j+g) = a(j)
        j = j - g
        cnt += 1
      }
      a(j+g) = v
    }
    (a, cnt)
  }

  def gapArray(max: Int): Array[Int] = {
    def loop(buff: ListBuffer[Int] = ListBuffer.empty[Int], i: Int = 1): Array[Int] = {
      if(max < i) buff.toArray.reverse
      else loop(buff += i, i*3+1)
    }
    
    loop()
  }
  
  def shellSort(a: Array[Int], n: Int): (Array[Int], Array[Int], Int) = {
    var cnt = 0
    /*
    var m = 4
    var g: Array[Int] = (m to 0 by - 1).map(p => p).toArray
    */
    var g: Array[Int] = gapArray(a.length)
    var m = g.size
    //println("g:" + g.mkString(" "))
    (0 until m).foreach{ i =>
      val (_, rcnt) = insertionSort(a, n, g(i))
      cnt = cnt + rcnt
    }
    
    (a, g, cnt)
  }
  
  val rshell    = shellSort(ra.clone(), rn)

  //println("ra:" + ra.toList.mkString(" "))
  println(rshell._2.size)
  println(rshell._2.toList.mkString(" "))
  println(rshell._3)

  rshell._1.toList.foreach(println)

}
