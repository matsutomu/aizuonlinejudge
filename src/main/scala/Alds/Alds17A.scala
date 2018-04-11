package Alds


import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Alds17A extends App {

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
    if(input(index).right != nil){ // brother
      setDepth(input(index).right, cnt)
    }
    if(input(index).left != nil){ // child
      setDepth(input(index).left, cnt + 1)
    }
  }
  
  def getChild(index: Int): Array[Int] = {
    if(input(index).left == nil){
      Array.empty[Int]
    } else {
      var node = input(input(index).left)
      var ret = ArrayBuffer(input(index).left)
      while(node.right != nil){
        ret += node.right
        node = input(node.right)
      }
      ret.toArray
    }
  }
  
  val n = StdIn.readLine().trim.toInt
  val input = Array.fill[Node](n)(null)
  val depth = Array.fill[Int](n)(0)

  (0 until n).foreach{ _ =>
    val a = StdIn.readLine().trim.split(" ").map(_.toInt)
    val i = a(0)
    val c = a.drop(2)
    var before: Int = -1
    input(i) = if(input(i) == null) Node(nil, nil, nil) else input(i)
    c.zipWithIndex.foreach{ ci =>
      input(ci._1) = if(input(ci._1) == null) Node(i, nil, nil) else input(ci._1)
      
      if(ci._2 == 0) {
        input(i).left =  ci._1
      }
      
      if(before != nil){
        input(before).right = ci._1
      }
      before = ci._1
      input(ci._1).parent = i
    }
    
    
  }

  var root = nil
  (0 until n).foreach{ i =>
    //println(s"$i:" + input(i))
    if(input(i).parent == nil) root = i
  }
  
  setDepth(root, 0)
  
  (0 until n).foreach{ i =>
    val ch = getChild(i)
    println(s"node $i: " + input(i).nodeLiteral(depth(i), ch))
  }

}

/*
1
0 0
node 0: parent = -1, depth = 0, root, []

13
0 3 1 4 10
1 2 2 3
2 0
3 0
4 3 5 6 7
5 0
6 0
7 2 8 9
8 0
9 0
10 2 11 12
11 0
12 0


node 0: parent = -1, depth = 0, root, [1, 4, 10]
node 1: parent = 0, depth = 1, Internal node, [2, 3]
node 2: parent = 1, depth = 2, leaf, []
node 3: parent = 1, depth = 2, leaf, []
node 4: parent = 0, depth = 1, Internal node, [5, 6, 7]
node 5: parent = 4, depth = 2, leaf, []
node 6: parent = 4, depth = 2, leaf, []
node 7: parent = 4, depth = 2, Internal node, [8, 9]
node 8: parent = 7, depth = 3, leaf, []
node 9: parent = 7, depth = 3, leaf, []
node 10: parent = 0, depth = 1, Internal node, [11, 12]
node 11: parent = 10, depth = 2, leaf, []
node 12: parent = 10, depth = 2, leaf, []


7
0 0
1 3 2 6 0
2 2 3 4
3 0
4 0
5 0
6 1 5
node 0: parent = 1, depth = 1, leaf, []
node 1: parent = -1, depth = 0, root, [2, 6, 0]
node 2: parent = 1, depth = 1, internal node, [3, 4]
node 3: parent = 2, depth = 2, leaf, []
node 4: parent = 2, depth = 2, leaf, []
node 5: parent = 6, depth = 2, leaf, []
node 6: parent = 1, depth = 1, internal node, [5]

11
6 4 7 8 9 10
0 2 6 1
1 1 2
2 1 3
3 1 4
4 1 5
5 0
7 0
8 0
9 0
10 0
node 0: parent = -1, depth = 0, root, [6, 1]
node 1: parent = 0, depth = 1, internal node, [2]
node 2: parent = 1, depth = 2, internal node, [3]
node 3: parent = 2, depth = 3, internal node, [4]
node 4: parent = 3, depth = 4, internal node, [5]
node 5: parent = 4, depth = 5, leaf, []
node 6: parent = 0, depth = 1, internal node, [7, 8, 9, 10]
node 7: parent = 6, depth = 2, leaf, []
node 8: parent = 6, depth = 2, leaf, []
node 9: parent = 6, depth = 2, leaf, []
node 10: parent = 6, depth = 2, leaf, []

 */
