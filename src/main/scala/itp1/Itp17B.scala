package itp1

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp17B extends App {

  var Array(n, x) = Array(-1, -1)
  var count = 0
  val buff = ListBuffer.empty[Int]
  
  while(n != 0 || x != 0){
    val Array(inn, inx) = StdIn.readLine().split(' ').map(_.toInt)
    n = inn
    x = inx
    count = 0
    (1 to n).foreach{ h =>
      (h+1 to n).foreach{ i =>
        (i+1 to n).foreach{ k =>
          count += (if(h + i + k == x) 1 else 0)
        }
      }
    }
    
    buff += count
  }
  
  buff.dropRight(1).foreach(println)
  
}

