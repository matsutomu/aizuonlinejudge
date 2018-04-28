package itp1

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp19D extends App {

  var text = StdIn.readLine()
  val n = StdIn.readLine().toInt
  val buff = ListBuffer.empty[String]
  
  def divide(target: String, s: Int, e: Int): Array[String] = {
    Array(target.take(s), target.slice(s, e), target.takeRight(target.length - e))
  }
  
  (0 until n).foreach { _ =>
    val cmd = StdIn.readLine().split(' ')
    
    val a = cmd(1).toInt
    val b = cmd(2).toInt + 1
    
    val divs = divide(text, a, b)
    //println(divs.toList)
    text = cmd(0) match{
      case "replace" => divs(0) + cmd(3) + divs(2)
      case "reverse" => divs(0) + divs(1).reverse + divs(2)
      case _ => 
        buff +=  divs(1)
        text
    }
  }
  
  buff.foreach(println)
  
}
