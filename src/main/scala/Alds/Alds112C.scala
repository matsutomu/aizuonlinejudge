package Alds

import scala.collection.immutable.List
import scala.io.StdIn

object Alds112C extends App {


  val n = StdIn.readLine().toInt
  //val M = Array.fill[ArrayBuffer[(Int, Int)]](n)(ArrayBuffer.empty[(Int, Int)])
  val M = Array.fill[List[(Int, Int)]](n)(List.empty[(Int, Int)])

  (0 until n).foreach { _ =>
    val a = StdIn.readLine().split(' ').map(_.toInt)
    M(a(0)) = (0 until a(1)).map { k =>
      (a(2 + k * 2), a(2 + k * 2 + 1))
    }.reverse.toList
  }

  //(0 until n).foreach { i => println(M(i).mkString("")) }
  private val WHITE = 0
  private val GRAY = 1
  private val BLACK = 9

  val color = Array.fill[Int](n)(WHITE)
  val distance = Array.fill(n)(Int.MaxValue)
  val parent = Array.fill(n)(-1)

  val pq = scala.collection.mutable.PriorityQueue.empty[(Int, Int)]

  def dijkstra(s: Int): Unit = {
    distance(s) = 0
    pq.enqueue((0, 0))
    //color(s) = GRAY

    while (pq.nonEmpty) {
      
      val f = pq.dequeue() // (cost, index)
      val u = f._2 // index
      
      //color(u) = BLACK
      if (distance(u) >= -1 * f._1) { 
        M(u).foreach{ v =>
          //if (color(v._1) != BLACK) {
            if (distance(u) + v._2 < distance(v._1)) {
              distance(v._1) = distance(u) + v._2
              //color(v._1) = GRAY
              pq.enqueue((-1 * v._2, v._1)) // (cost , index)
            }
         // }
        }
      }
    }
  }

  dijkstra(0)

  (0 until n).foreach(i => println(i + " " + distance(i)))

}

