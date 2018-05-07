package Alds


import scala.io.StdIn

object Alds112A extends App {

  val n = StdIn.readLine().toInt
  val M = Array.fill[Int](n, n)(0)

  (0 until n).foreach { i =>
    M(i) = StdIn.readLine().trim().split(' ').map(_.toInt)
  }

  val color = Array.fill[Int](n)(0)
  val distance = Array.fill(n)(Int.MaxValue)
  val parent = Array.fill(n)(-1)
  
  private val WHITE = 0
  private val GRAY = 1
  private val BLACK = 9
  
  var mincost = -1
  var u = 0
  distance(0) = 0
  while(mincost != Int.MaxValue){
    mincost = Int.MaxValue
    (0 until n).foreach{ i =>
      if(color(i) != BLACK && distance(i) < mincost){
        mincost = distance(i)
        u = i
      }
    }

    if(mincost != Int.MaxValue){
      color(u) = BLACK
      
      (0 until n).foreach{ v =>
        if(color(v) != BLACK && M(u)(v) != -1){
          if(M(u)(v) < distance(v)){
            distance(v) = M(u)(v)
            parent(v) = u
            color(v) = GRAY
          }
        }
      }
      
    }

  }

  println(distance.sum)

}

