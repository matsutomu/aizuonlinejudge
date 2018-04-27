package itp1

import scala.io.StdIn

object Itp19A extends App {

  private val w = StdIn.readLine()
  var text = ""
  var cnt = 0
  
  do {
    cnt += text.toLowerCase.split(' ').count(p => p == w)
    text = StdIn.readLine()
  } while(text != "END_OF_TEXT")
  
  println(cnt)
  
}

/*
  def loop(index: Int, target: String): Unit = {
    if(index >= 0){
      val next = target.indexOf(w,index)
      if(next >= 0) {
        cnt +=1
        loop(next+wsize, target)
      }
    }
  }
 */