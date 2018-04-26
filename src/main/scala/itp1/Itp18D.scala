package itp1

import scala.io.StdIn

object Itp18D extends App {

  private val s = StdIn.readLine()
  private val p = StdIn.readLine()
  
  println(if((s+s).contains(p)) "Yes" else "No")
  
}


/*
  // NG!
  
  private val stsize = s.size 
  var i = 0
  
  def search(i: Int): Boolean = {
    if(i > stsize) {
      false
    } else {
      if((s.slice(i, stsize + i) + s.slice(0, i)).indexOf(p) >= 0){
        true
      } else {
        search(i+1)
      }
    }
  }
  
  println(if(search(0)) "Yes" else "No")

 */