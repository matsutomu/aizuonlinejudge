package Alds

import scala.io.StdIn

object Alds17C extends App {

  final val nil = -1
  
  case class Node(var parent: Int = -1,var left: Int = -1, var right: Int = -1)

  val n = StdIn.readLine().trim.toInt
  val input  = Array.fill[Node](n)(Node())
  val depth  = Array.fill[Int](n)(0)
  val height = Array.fill[Int](n)(0)

  (0 until n).foreach{ _ =>
    val Array(current, left, right) = StdIn.readLine().trim.split(" ").map(_.toInt)
    input(current).left = left
    input(current).right = right
    if(left != nil) input(left).parent = current
    if(right != nil) input(right).parent = current
  }

  var root = nil
  (0 until n).foreach{ i =>
    //println(s"$i:" + input(i))
    if(input(i).parent == nil) root = i
  }
  
  def preorder(index: Int): Unit = {
    if(index != nil) {
      print(" " + index)
      preorder(input(index).left)
      preorder(input(index).right)
    }
  }

  def inorder(index: Int): Unit = {
    if(index != nil){
      inorder(input(index).left)
      print(" " + index)
      inorder(input(index).right)
    }
  }

  def postorder(index: Int): Unit = {
    if(index != nil) {
      postorder(input(index).left)
      postorder(input(index).right)
      print(" " + index)
    }
  }
  
  println("Preorder")
  preorder(root)
  println("")

  println("Inorder")
  inorder(root)
  println("")

  println("Postorder")
  postorder(root)
  println("")

}

/*
9
0 1 4
1 2 3
2 -1 -1
3 -1 -1
4 5 8
5 6 7
6 -1 -1
7 -1 -1
8 -1 -1
Preorder
 0 1 2 3 4 5 6 7 8
Inorder
 2 1 3 0 6 5 7 4 8
Postorder
 2 3 1 6 7 5 8 4 0


 
 
 */
