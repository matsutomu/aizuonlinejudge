package itp1


import scala.annotation.tailrec
import scala.io.StdIn

object Itp13C extends App {
  
  @tailrec def loop(x: Int, y: Int): Unit = {
    if(x == 0 && y == 0) ()
    else {
      if(x <= y) println(x + " " + y)
      else println(y + " " + x)

      val a = StdIn.readLine().trim.split(' ').map(_.toInt)
      loop(a(0), a(1))
    } 
  }

  val f = StdIn.readLine().trim.split(' ').map(_.toInt)

  loop(f(0), f(1))

}

/* too late
var continue = true
var result = new StringBuilder
while(continue){
  val Array(x, y) = StdIn.readLine().trim.split(' ').map(_.toInt)
  continue = if(x == 0 && y == 0) {
    false
  } else {
    result.append(if(x > y) f"$y $x%n" else f"$x $y%n")
    true
  }
}

print(result)
*/

