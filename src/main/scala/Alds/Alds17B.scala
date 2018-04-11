package Alds

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Alds17B extends App {

  final val nil = -1
  
  case class Node(var parent: Int = -1,var left: Int = -1, var right: Int = -1){
    
    def typeInfo: String = if(parent == -1) {
      "root"
    } else if(left == nil ) {
      "leaf"
    } else {
      "internal node"
    }
    
    def nodeLiteral(depth: Int, childs: Array[Int]): String = {
      s"parent = $parent, depth = $depth, $typeInfo, [${childs.mkString(", ")}]"
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
    height(index) = h1 + h2
    h1 + h2
  }
  
  def getChild(index: Int): Array[Int] = (input(index).left, input(index).right) match {
    case (-1,      -1) => Array.empty[Int]
    case (left,    -1) => Array(left)
    case (-1,   right) => Array(right)
    case (left, right) => Array(left, right)
  }
  
  val n = StdIn.readLine().trim.toInt
  val input  = Array.fill[Node](n)(null)
  val depth  = Array.fill[Int](n)(0)
  val height = Array.fill[Int](n)(0)

  (0 until n).foreach{ _ =>
    val Array(current, left, right) = StdIn.readLine().trim.split(" ").map(_.toInt)
    input(current) = if(input(current) == null) Node(nil, left, right) else input(current)
    input(left) = if(input(left) == null) Node(current, nil, nil) else input(left)
    input(right) = if(input(right) == null) Node(current, nil, nil) else input(right)
    
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
    println(s"node $i: " + input(i).nodeLiteral(depth(i), ch))
  }

}

/*


 */
