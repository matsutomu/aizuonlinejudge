package Alds

import scala.io.StdIn

object Alds13D extends App {
  
  var index = -1
  val inp = StdIn.readLine().trim
  val result = inp.foldLeft(List.empty[Int], List.empty[(Int, Int)], 0){
    case ((allstack, eachstack, allmenseki), e) =>
    //println(allstack)
    //println(eachstack)
    index += 1
    if(e == '\\') {
      (index :: allstack, eachstack, allmenseki)  
    } else if(e == '/' && allstack.nonEmpty){
      var j = allstack.head
      var men = index - j
      var a = index - j
      var temp = eachstack
      while(temp.nonEmpty && temp.head._1 > j){
        a += temp.head._2
        temp = temp.tail
      }
      temp = (j, a) :: temp
      (if(allstack.length == 1) Nil else allstack.tail,
        temp, allmenseki + men)
    } else (allstack, eachstack, allmenseki)

  }
  
  println(result._3)
  println(result._2.size + (if(result._2.isEmpty) "" else
    " " + result._2.reverse.map(e => e._2).mkString(" ")))
  
}
