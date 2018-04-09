package Alds

import scala.io.StdIn

object Alds15D extends App {


  val rn = StdIn.readLine().trim.toInt
  val ina = StdIn.readLine().trim.split(' ').map(_.toInt)
  
  final val infity = Int.MaxValue
  def mergeSort(): Long = {
    @inline def copyToArray(start: Int, end: Int): Array[Int] = {
      val alsize = end - start + 1
      val temp = new Array[Int](alsize)
      Array.copy(ina, start, temp, 0, alsize - 1)
      temp(alsize-1) = infity
      temp
    }

    def merge(left: Int, mid: Int, right: Int): Long ={
      val al = copyToArray(left, mid)
      val ar = copyToArray(mid, right)
      var i = 0
      var j = 0
      var cnt = 0L
      //println("left:" + al.toList)
      //println("right:" + ar.toList)
      (left until right).foreach{ k =>
        if(al(i) <= ar(j)){
          ina(k) = al(i)
          i += 1
        } else {
          ina(k) = ar(j)
          j += 1
          cnt += mid - left - i
        }
      }
      //println("all:" + ar2.toList.mkString(" "))
      cnt
    }

    def innerMergeSort(left: Int, right: Int): Long ={
      
      if(left+1 < right){
        val mid = (left + right) / 2
        val rleft = innerMergeSort(left, mid)
        val rright = innerMergeSort(mid, right)
        val rmerge = merge(left, mid, right)
        
        rleft + rright + rmerge
      } else {
        0L
      }
    }

    innerMergeSort(0, rn)

  }
  
  println(mergeSort())

  
}

