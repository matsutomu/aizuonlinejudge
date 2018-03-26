package Alds

import scala.io.StdIn

object Alds13A extends App {

  val lst = StdIn.readLine().trim.split(' ').toList
  val enzan = Map("+" -> 1, "-" -> 1, "*" -> 2)
  
  val r = lst.foldLeft(List.empty[Int]){ (stack, e) =>
    val o = enzan.getOrElse(e, 0)
    if(o != 0){
      val ope1 = stack.head
      val ope2 = stack.tail.head
      e match {
        case "+" => (ope1 + ope2) :: stack.drop(2)
        case "-" => (ope2 - ope1) :: stack.drop(2)
        case "*" => (ope1 * ope2) :: stack.drop(2)
      }
      
    } else {
      e.toInt :: stack
    }
  }
  
  println(r.head)

}
