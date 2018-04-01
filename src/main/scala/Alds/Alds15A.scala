package Alds

import scala.collection.mutable.ArrayBuffer

object Alds15A extends App {

  private val buf = new Array[Byte](1024)
  //private val input = ListBuffer.empty[Array[Int]]
  private val input = ArrayBuffer.empty[Array[Int]]

  def isChar(c: Int) = 32 <= c && c <= 126
  
  def reads(): Unit = {
    var sb = new StringBuilder
    var insize = System.in.read(buf)
    var i = 0
    while(i < insize){
      val c = buf(i)
      if(isChar(c)) {
        sb.append(c.toChar.toString)
        if(i + 1 == insize) {
          input += sb.toString().split(' ').map(_.toInt)
          sb.clear()
        }
      } else { // break
        input += sb.toString().split(' ').map(_.toInt)
        sb.clear()
      }
      i += 1
    }
  }
  
  reads()
  
  val n = input(0)(0)
  val a = input(1)
  val q = input(2)(0)
  val m = input(3)

  @inline def solve(i: Int, m: Int): Boolean = if(m == 0) true
  else if(i >= n) false
  else solve(i+1, m) || solve(i+1, m - a(i))

  val result = m.map(p => if(solve(0, p)) "yes" else "no")

  //print(result.mkString(f"%n") + f"%n")
  result.foreach(println)
}

  /*
  import scala.io.StdIn
  val sc = new java.util.Scanner(System.in)
  val n = sc.nextInt 
  val a = Array.fill(n)(sc.nextInt) 
  val q = sc.nextInt 
  val m = Array.fill(q)(sc.nextInt)

  def solve(i: Int, m: Int): Boolean = if(m == 0) true
    else if(i >= n) false
    else solve(i+1, m) || solve(i+1, m - a(i))
  
  val result = m.map(p => if(solve(0, p)) "yes" else "no")

  print(result.mkString(f"%n") + f"%n")

  */

