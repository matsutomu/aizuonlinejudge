package itp1

import scala.io.StdIn

object Itp18A extends App {

  val result = StdIn.readLine().map(f => if(f.isUpper) f.toLower else f.toUpper)
  /*
  val literal = StdIn.readLine()
  val result = literal.map{ x =>
    if(x.isLetter && x.isLower) x.toUpper
    else if (x.isLetter && x.isUpper) x.toLower
    else x
  }*/
  
  println(result)
}

