package itp1

import scala.io.StdIn

object Itp19B extends App {

  var text = ""
  do {
    if(text != ""){
      var shuffleCnt = Integer.parseInt(StdIn.readLine())

      (0 until shuffleCnt).foreach{ _ =>
        val splitAt = Integer.parseInt(StdIn.readLine())
        text = text.slice(splitAt, text.length) + text.slice(0, splitAt)
      }
      println(text)
    }
    text = StdIn.readLine()
  } while(text != "-")
  
}

/*
aabc
3
1
2
1
vwxyz
2
3
4
-


aabc
3 caab
1 aabc
2 bcaa
1 caab

aaaax
1 aaaxa
1

 */
