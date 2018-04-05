package Alds

import scala.io.StdIn

object Alds16C extends App {
  
  case class Card(mark: String, number: Int){
    override def toString: String = f"$mark $number%n"
  }
  val n = StdIn.readLine().trim.toInt
  val ar1 = new Array[Card](n)
  val ar2 = new Array[Card](n)
  (0 until n).foreach{ i =>
    val Array(m, num) = StdIn.readLine().trim.split(' ')
    ar1(i) = Card(m, num.toInt)
    ar2(i) = Card(m, num.toInt)
  } 


  def quickSort(target: Array[Card], partIndex: Int, size: Int): Unit = {

    val last = target(size)

    @inline def swap(from: Int, to: Int): Unit = {
      //println(s"from: $from  to: $to")
      val temp = target(from)
      target(from) = target(to)
      target(to)   = temp
    }

    def partition(arr: Array[Card], p: Int, r: Int): Int = {
      var i = p - 1
      (p until r).foreach{ j =>
        if(arr(j).number <= last.number){
          i += 1
          swap(i, j)
          //println(a.toList.mkString(" "))
        }
      }
      swap(i+1, r)
      //println("last:" + a.toList.mkString(" "))
      i + 1
    }

    if(partIndex < size){
      val newIndex = partition(target, partIndex, size)
      quickSort(target, partIndex, newIndex - 1)
      quickSort(target, newIndex + 1, size)
    }
    
    
  }
  //println(s"num: $partitionIndex")

  final val infity = Int.MaxValue
  def mergeSort(target: Array[Card]): Unit = {
    var compareCount = 0

    @inline def copyToArray(start: Int, end: Int): Array[Card] = {
      val alsize = end - start + 1
      val ar = new Array[Card](alsize)
      Array.copy(target, start, ar, 0, alsize - 1)
      ar(alsize-1) = Card("", infity)
      ar
    }

    def merge(left: Int, mid: Int, right: Int): Unit ={
      //var al = a.slice(left, mid) ++ Array(infity)
      val al = copyToArray(left, mid)

      //var ar = a.slice(mid, right) ++ Array(infity)
      val ar = copyToArray(mid, right)
      var i = 0
      var j = 0
      //println("left:" + al.toList)
      //println("right:" + ar.toList)
      (left until right).foreach{ k =>
        compareCount += 1
        if(al(i).number <= ar(j).number){
          target(k) = al(i)
          i += 1
        } else {
          target(k) = ar(j)
          j += 1
        }
      }
      //println("all:" + a.toList.mkString(" "))
    }

    def innerMergeSort(left: Int, right: Int): Unit ={
      if(left+1 < right){
        val mid = (left + right) / 2
        innerMergeSort(left, mid)
        innerMergeSort(mid, right)
        merge(left, mid, right)
      }
    }

  }



  quickSort(ar1, 0, n - 1)
  mergeSort(ar2)
  
  var same = false
  (ar1 zip ar2).foreach{ p =>
    if(p._1 == p._2){
      same = true
    }
  }
  
  println(if(same) "Stable" else "Not stable")
  
  print(ar1.mkString(""))
  
}

/*
NG case

20
S 10000000
C 7777
H 2500000
D 2500000
H 2
D 999999999
S 999999999
H 10000000
H 7777
S 2500000
C 999999999
C 2500000
D 7777
D 10000000
D 2
S 2
C 2
H 999999999
S 7777
C 10000000


Not stable
H 2
D 2
S 2
C 2
C 7777
H 7777
D 7777
S 7777
C 2500000
D 2500000
H 2500000
S 2500000
H 10000000
D 10000000
S 10000000
C 10000000
C 999999999
H 999999999
S 999999999
D 999999999

 */