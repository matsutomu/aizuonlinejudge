package Alds

import scala.io.StdIn

object Alds17D extends App {

  val n = StdIn.readLine().trim.toInt
  
  val preorders = StdIn.readLine().split(' ').map(_.toInt)
  val inorders = StdIn.readLine().split(' ').map(_.toInt)
  val postorders = Array.fill[Int](n)(0)
  
  var position = 0
  var positionResult = 0

  def reconstruction(left: Int, right: Int): Unit = {
    if(left < right){
      val current = preorders(position)
      position += 1
      val middleIdx = inorders.indexOf(current)
      reconstruction(left, middleIdx)
      reconstruction(middleIdx + 1, right)
      postorders(positionResult) = current
      positionResult += 1
      
    } else ()
  }
  
  reconstruction(0, n)
  
  println(postorders.mkString(" "))
  
}

/*
 
 */
