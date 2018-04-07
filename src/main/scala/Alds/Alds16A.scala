package Alds

import scala.io.StdIn

object Alds16A extends App {
  
  val n = StdIn.readLine().trim.toInt
  val ar = StdIn.readLine().trim.split(' ').map(_.toInt)
  val result = new Array[Int](n)
  var max = ar.max
  
  def countingSort(): Unit = {
    val count = Array.fill(max + 1)(0)

    ar.foreach{ e =>
      count(e) += 1
    }

    def loop(current: Int): Unit = {
      if(current <= max){
        count(current) = count(current) + count(current - 1)
        loop(current + 1)
      } 
    }

    loop(1)

    //println(count.mkString(" "))
    
    (n-1 to 0 by -1).foreach{ i =>
      //print(s"ar($i) : ${ar(i)} ")
      //println(s"count(ar(i)) : ${count(ar(i))} ")
      result(count(ar(i)) - 1) = ar(i)
      count(ar(i)) -= 1
    }
  }
  
  countingSort()
  
  println(result.mkString(" "))

}

/*

7
2 5 1 3 2 3 0
0 1 2 2 3 3 5

8
8 7 6 5 4 3 2 1

 */