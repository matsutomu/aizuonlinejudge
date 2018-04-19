package itp1

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp17A extends App {

  val result = ListBuffer.empty[String]
  var Array(m, f, r) = Array(0, 0, 0)
  while(m != -1 || f != -1 || r != -1){
    val Array(inm, inf, inr) = StdIn.readLine().split(' ').map(_.toInt)
    m = inm
    f = inf
    r = inr
    val rank = (m, f, r) match {
      case (-1, -1, -1) =>  ""
      case (-1,  y, z) =>  "F"
      case ( x, -1, z) =>  "F"
      case ( x,  y, z) if x + y >= 80 =>  "A"
      case ( x,  y, z) if (65 <= x + y ) && (x + y < 80)  =>  "B"
      case ( x,  y, z) if (50 <= x + y ) && (x + y < 65)  =>  "C"
      case ( x,  y, z) if (30 <= x + y ) && (x + y < 50)  =>  if (50 <= z) "C" else "D"
      case ( x,  y, z) if x + y < 30  =>  "F"
      case _ => ""
    }
    result += rank
    
  }
    
  print(result.mkString(f"%n"))
  //result.foreach(println)
  
}

