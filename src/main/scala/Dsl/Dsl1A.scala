package Dsl

import scala.io.StdIn

object Dsl1A extends App {


  case class DisjointSet(size: Int){
    val rank: Array[Int] = Array.fill[Int](size)(0)
    val pointer: Array[Int]  = Array.fill[Int](size)(0)
    
    (0 until size).foreach(i => makeSet(i))
    
    def makeSet(x: Int): Unit = {
      pointer(x) = x
      rank(x) = 0
    }
    
    def same(x: Int, y: Int): Boolean = {
      findSet(x) == findSet(y)
    }
    
    def unite(x: Int, y: Int): Unit = {
      link(findSet(x), findSet(y))
    }
    
    def link(x: Int, y: Int): Unit = {
      if(rank(x) > rank(y)){
        pointer(y) = x
      } else {
        pointer(x) = y
        if(rank(x) == rank(y)){
          rank(y) +=1
        }
      }
    }
    
    def findSet(x: Int): Int = {
      if(x != pointer(x)){
        pointer(x) = findSet(pointer(x))
      }
      pointer(x)
    }
    
  }
  
  
  val Array(n, q) = StdIn.readLine().split(' ').map(_.toInt)
  val disj = DisjointSet(n)
  
  (0 until q).foreach{ _ =>
    val Array(command, x, y) = StdIn.readLine().split(' ').map(_.toInt)
    if(command == 0) {
      disj.unite(x, y)
    } else {
      println(if(disj.same(x, y)) "1" else "0")
    }
  }
  
  

}

