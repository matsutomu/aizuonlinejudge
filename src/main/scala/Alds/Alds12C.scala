package Alds

import scala.io.StdIn
object Alds12C extends App {

  val rn = StdIn.readLine().trim.toInt
  case class Card(suit: Char, num: Int){
    override def toString: String = s"$suit$num"
  }
  val ra = StdIn.readLine().trim.split(' ').map{ p =>
    Card(p.charAt(0), p.charAt(1).toString.toInt)
  }
  val ra1 = ra.clone()
  val ra2 = ra.clone()
  
  def isStable(a: Array[Card], after: Array[Card], n: Int): Boolean = {
    (0 until n).foreach{ i =>
      if(a(i).suit != after(i).suit) return false
    }
    true
  }
  
  def bubbleSort(a: Array[Card], n: Int): Array[Card] = {
    (0 until n).foreach{ i =>
      // println(s"bubble($i):" + a.toList)
      (n-1 until i by -1).foreach{ j =>
        if(a(j).num < a(j-1).num){
          val temp = a(j)
          a(j) = a(j-1)
          a(j-1) = temp
        }
      }
    }
    a
  }

  def selectionSort(a: Array[Card], n: Int): Array[Card] = {
    (0 until n).foreach{ i =>
      // println(s"selection($i):" + a.toList)

      var minj = i
      (i until n).foreach{ j =>
        minj = if(a(j).num < a(minj).num) j else minj
      }
      val temp = a(minj)
      a(minj) = a(i)
      a(i) = temp
    }
    a
  }
  
  val rbubble    = bubbleSort(ra1, rn)
  val rselection = selectionSort(ra2, rn)

  //println("ra:" + ra.toList.mkString(" "))
  println(rbubble.toList.mkString(" "))
  println("Stable")

  println(rselection.toList.mkString(" "))
  println(if(isStable(rbubble, rselection, rn)) "Stable" else "Not stable")

}
