package itp1

import scala.io.StdIn

object Itp14C extends App {
  
  var continue = true
  while(continue){
    val Array(a, op, b) = StdIn.readLine().split(' ')
    val result = op match{
      case "+" => a.toInt + b.toInt
      case "-" => a.toInt - b.toInt
      case "*" => a.toInt * b.toInt
      case "/" => a.toInt / b.toInt
      case _ => {
        continue = false
        0
      } 
    }
    if(continue){
      println(f"$result")
    }
  }

}
