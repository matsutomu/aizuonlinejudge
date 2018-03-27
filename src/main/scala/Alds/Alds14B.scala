package Alds


import scala.io.StdIn

object Alds14B extends App {

  val n = StdIn.readLine().trim.toInt
  val lst1 = StdIn.readLine().trim.split(' ').map(_.toInt)
  val t = StdIn.readLine().trim.toInt
  val lst2 = StdIn.readLine().trim.split(' ').map(_.toInt)

  def search(key: Int): Boolean = {
    var left = 0
    var right = n
    var ret = false
    while(left < right && !ret) {
      val mid = (right + left) / 2
      if (lst1(mid) == key) {
        ret = true
      } else if(key < lst1(mid)) {
        right = mid
      } else {
        left = mid + 1
      }
    }
    ret
  }

  val result = lst2.foldLeft(0){ (acc, e) =>

    if(search(e)) acc + 1 else acc
  }
  
  println(result)

}
