package Alds

import scala.io.StdIn

object Alds17B extends App {

  final val nil = -1
  
  case class Node(var parent: Int = -1,var left: Int = -1, var right: Int = -1){
    
    def typeInfo: String = if(parent == -1) {
      "root"
    } else if(left == nil && right == nil) {
      "leaf"
    } else {
      "internal node"
    }
    
    def nodeLiteral(depth: Int, sibling: Int, height: Int): String = {
      val degree = (if(left == nil) 0 else 1) + (if(right == nil) 0 else 1)
      s"parent = $parent, sibling = $sibling, degree = $degree, depth = $depth, height = $height, $typeInfo"
    }
    override def toString: String = s"parent = $parent, depth = X, $typeInfo, [child]"
    
  }

  def setDepth(index: Int, cnt: Int): Unit = {
    //println("setDepth:" + index)
    depth(index) = cnt
    if(input(index).right != nil){ 
      setDepth(input(index).right, cnt + 1)
    }
    if(input(index).left != nil){ 
      setDepth(input(index).left, cnt + 1)
    }
  }
  
  def setHeight(index: Int): Int = {
    //println("setDepth:" + index)
    val h1 = if(input(index).right != nil){
      setHeight(input(index).right) + 1
    } else 0
    val h2 = if(input(index).left != nil){
      setHeight(input(index).left) + 1
    } else 0
    height(index) = scala.math.max(h1, h2)
    height(index)
  }
  
  def getChild(index: Int): Array[Int] = (input(index).left, input(index).right) match {
    case (-1,      -1) => Array.empty[Int]
    case (left,    -1) => Array(left)
    case (-1,   right) => Array(right)
    case (left, right) => Array(left, right)
  }
  
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
  
  setDepth(root, 0)
  setHeight(root)
  
  (0 until n).foreach{ i =>
    val ch = getChild(i)
    val parent = input(i).parent
    val sibling = if(parent != nil){
      if(input(parent).left == i) input(parent).right
      else input(parent).left
    } else -1
    
    println(s"node $i: " + input(i).nodeLiteral(depth(i), sibling, height(i)))
  }

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

node 0: parent = -1, sibling = -1, degree = 2, depth = 0, height = 3, root
node 1: parent = 0, sibling = 4, degree = 2, depth = 1, height = 1, internal node
node 2: parent = 1, sibling = 3, degree = 0, depth = 2, height = 0, leaf
node 3: parent = 1, sibling = 2, degree = 0, depth = 2, height = 0, leaf
node 4: parent = 0, sibling = 1, degree = 2, depth = 1, height = 2, internal node
node 5: parent = 4, sibling = 8, degree = 2, depth = 2, height = 1, internal node
node 6: parent = 5, sibling = 7, degree = 0, depth = 3, height = 0, leaf
node 7: parent = 5, sibling = 6, degree = 0, depth = 3, height = 0, leaf
node 8: parent = 4, sibling = 5, degree = 0, depth = 2, height = 0, leaf

7
0 -1 -1
1 -1 4
4 5 -1
2 3 1
3 -1 0
6 -1 -1
5 6 -1
node 0: parent = 3, sibling = -1, degree = 0, depth = 2, height = 0, leaf
node 1: parent = 2, sibling = 3, degree = 1, depth = 1, height = 3, internal node
node 2: parent = -1, sibling = -1, degree = 2, depth = 0, height = 4, root
node 3: parent = 2, sibling = 1, degree = 1, depth = 1, height = 1, internal node
node 4: parent = 1, sibling = -1, degree = 1, depth = 2, height = 2, internal node
node 5: parent = 4, sibling = -1, degree = 1, depth = 3, height = 1, internal node
node 6: parent = 5, sibling = -1, degree = 0, depth = 4, height = 0, leaf

 */
