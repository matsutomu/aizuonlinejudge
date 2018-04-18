package Alds

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Alds18A extends App {

  val n = StdIn.readLine().trim.toInt
  val buff = ListBuffer.empty[String]
  case class Node(key: Int, var parent: Node, var left: Node, var right: Node){
    private def getDefaultKey(p: Node) = if(p == null) -1 else p.key
    override def toString: String = s"Node($key, ${getDefaultKey(parent)} ${getDefaultKey(left)} ${getDefaultKey(right)})"
  }
  var root: Node = _
  
  (0 until n).foreach { _ =>
    val command = StdIn.readLine()
    command.split(' ') match {
      case Array("insert", key) => insert(Node(key.toInt, null, null, null))
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
  
  print(buff.mkString(""))
}

/*
 
 */
