package itp1

import scala.collection.mutable.ListBuffer

object Itp18C extends App {

  private val alphabets = Array.ofDim[Int](26)
  private val buff = ListBuffer.empty[String]
  val sc = new java.util.Scanner(System.in)
  while(sc.hasNext){
    buff += sc.next
  }
  
  buff.mkString("").foreach{ c =>
    val idx = c.toLower - 'a'
    if(idx >= 0) alphabets(idx) += 1
  }
  println(('a' to 'z').map(f => f.toString + " : " + alphabets(f - 'a')).mkString(f"%n"))
  
}


/*
ABCD E F Z
x 
y
z

 */