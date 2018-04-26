package Alds

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Alds18C extends App {

  val n = StdIn.readLine().trim.toInt
  var root: Node = _
  val buff = ListBuffer.empty[String]
  case class Node(key: Int, var parent: Node, var left: Node, var right: Node){
    private def getDefaultKey(p: Node) = if(p == null) -1 else p.key
    override def toString: String = s"Node($key, ${getDefaultKey(parent)} ${getDefaultKey(left)} ${getDefaultKey(right)})"
  }
  
  (0 until n).foreach { _ =>
    val command = StdIn.readLine()
    command.split(' ') match {
      case Array("insert", key) => insert(Node(key.toInt, null, null, null))
      case Array("find"  , key) => 
        buff += (if(find(root, key.toInt) == null) f"no%n" else f"yes%n")
      case Array("delete"  , key) => 
        println(key.toInt)
        delete(find(root, key.toInt))
      case _ => 
        // print
        inorder(root)
        buff += f"%n"
        preorder(root)
        buff += f"%n"
    }
  }
  
  
  def insert(z: Node): Unit = {
    var y:Node = null
    var x:Node = root
    while(x != null){
      y = x
      if(z.key < x.key){
        x = x.left
      } else {
        x = x.right
      }
    }
    z.parent = y
    
    if(y == null){
      root = z
    } else if(z.key < y.key){
      y.left = z
    } else {
      y.right = z
    }
  }
  
  def preorder(n: Node): Unit = {
    buff += " " + n.key
    if(n.left != null) preorder(n.left)
    if(n.right != null) preorder(n.right)
  }

  def inorder(n: Node): Unit = {
    if(n.left != null) inorder(n.left)
    buff += " " + n.key
    if(n.right != null) inorder(n.right)
  }
  
  def find(x: Node, key: Int): Node = {
    var temp = x
    while(temp != null && key != temp.key){
      if(key < temp.key){
        temp = temp.left
      } else {
        temp = temp.right
      }
    }
    temp
  }

  def delete(z: Node): Unit = {
    var temp = if(z.left == null || z.right == null) z else {
      getSuccessor(z)
    }
    
    var x = if(temp.left != null){
      temp.left
    } else {
      temp.right
    }
    
    if(x != null){
      x.parent = temp.parent
    }
    
    if(temp.parent == null) {
      root = x
    } else if(temp == temp.parent.left){
      temp.parent.left = x
    } else {
      temp.parent.right = x
    }

  }
  
  def getSuccessor(x: Node): Node = {
    if(x.right != null){
      minimum(x.right)
    } else {
      var temp1 = x
      var temp2 = x.parent
      while(temp2 != null && temp1 == temp2.right){
        temp1 = temp2
        temp2 = temp2.parent
      }
      temp2
    }
    
  }
  
  def minimum(x: Node): Node = {
    var temp = x
    while(temp.left != null){
      temp = temp.left
    }
    temp
  }

  print(buff.mkString(""))
}

/*
 
 */
