package Alds


import scala.io.StdIn

object Alds17A extends App {

  final val nil = -1
  
  case class Node(var parent: Int = -1,var left: Int = -1, var right: Int = -1){
    
    def typeInfo: String = if(parent == -1) {
      "root"
    } else if(left == nil ) {
      "leaf"
    } else {
      "Internal node"
    }

    override def toString: String = {
      s"parent = $parent, depth = X, $typeInfo, [child]"
    }
  }
  

  val n = StdIn.readLine().trim.toInt
  val input = Array.fill[Node](n)(Node())

  (0 until n).foreach{ i =>
    val a = StdIn.readLine().trim.split(" ").map(_.toInt)
    val c = a.drop(2)
    var before: Int = -1
    c.zipWithIndex.foreach{ ci =>
      val childNode = Node(i, nil, nil)
      input(ci._1) = childNode
      if(ci._2 == 0) input(i).left =  ci._1
      if(before != nil){
        input(before).right = ci._1
      }
      before = ci._1
    }
  }

  (0 until n).foreach{ i =>
    println(s"node $i: " + input(i).toString)
  }

}

