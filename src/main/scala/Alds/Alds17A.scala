package Alds


import scala.io.StdIn

object Alds17A extends App {

  sealed abstract class Node{
    val value: Int
    var parent: Node
    var left: Node
    var right: Node

    override def toString: String = {
      val p = if(parent == MyNil) -1 else parent.value
      val typeInfo = if(p == -1) {
        "root"
      } else if(left == MyNil ) {
        "leaf"
      } else {
        "Internal node"
      }
      
      s"node $value: parent = $p, $typeInfo, "
    }
  }
  
  case class NodeImpl(value: Int, var parent: Node, var left: Node, var right: Node) extends Node
  
  case object MyNil extends Node {
    val value: Int = -1
    var parent: Node = throw new RuntimeException
    var left: Node = throw new RuntimeException
    var right: Node = throw new RuntimeException
  }

  val n = StdIn.readLine().trim.toInt
  val input = Array.fill[Node](n)(MyNil)

  (0 until n).foreach{ i =>
    val cNode = NodeImpl(i, MyNil, MyNil, MyNil)
    input(i) = if(input(i) == MyNil) cNode else input(i)
    val a = StdIn.readLine().trim.split(" ").map(_.toInt)
    val c = a.drop(2)
    var before: Node = MyNil
    c.zipWithIndex.foreach{ ci =>
      val childNode = NodeImpl(ci._1, cNode, before, MyNil)
      input(ci._1) = childNode
      if(ci._2 == 0) input(i).left =  childNode
      if(before != MyNil){
        input(before.value).right = childNode
      }
      before = childNode
    }
  }

  input.toList.foreach(p => println(p.toString))

}

