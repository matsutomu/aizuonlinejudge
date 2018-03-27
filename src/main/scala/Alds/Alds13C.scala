package Alds

//import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Alds13C extends App {
  
  sealed abstract class Node {
    val key: Int
    var prev:Node
    var next: Node
  }

  case class Elements(key: Int, var prev:Node, var next: Node) extends Node {
    override def toString: String = s"Elements($key)"
  }

  case object MyNil extends Node{
    val key: Int = 0
    var prev: Node = MyNil
    var next: Node = MyNil
    override def toString: String = s"MyNil"
  }

  def insert(newVal: Int): Unit = {
    var x = Elements(newVal, MyNil, MyNil.next)
    MyNil.next.prev = x
    MyNil.next = x
  }

  def listSearch(searchVal: Int): Node = {
    var target = MyNil.next
    while(target.key != searchVal && target != MyNil){
      target = target.next
    }
    target
  }
  
  // nullを設定可能か？
  def deleteNode(target: Node): Unit = {
    if(target != MyNil){
      target.prev.next = target.next
      target.next.prev = target.prev
    }
  }

  def deleteFirst(): Unit = deleteNode(MyNil.next)

  def deleteLast(): Unit = deleteNode(MyNil.prev)

  def deleteKey(target: Int): Unit = deleteNode(listSearch(target))


  val n = StdIn.readLine().trim.toInt
  (0 until n).foreach{ _ =>
    val cmd = StdIn.readLine().trim.split(' ')
    if (cmd(0) == "deleteFirst") deleteFirst()
    else if(cmd(0) == "deleteLast") deleteLast()
    else if(cmd(0) == "insert") insert(cmd(1).toInt)
    else if(cmd(0) == "delete") deleteKey(cmd(1).toInt)
  }
  
  //println(commandList)
  val buff = ArrayBuffer.empty[Int]
  //val buff = ListBuffer.empty[Int]
  var current = MyNil.next
  while(current != MyNil){
    buff += current.key
    current = current.next
  }
  println(buff.mkString(" "))
}

/*** memo ***/

/*

#	Run#	Author	Rating	Date	Language	Version	Server	Time	Code
1	2201209	centillioncolors	67.62	2017-02-26 20:58	Scala	2.11.6	#7	01:19	1465 byte
2	2752384	matsutomu	0.00	2018-03-27 21:24	Scala	2.11.6	#7	01:69	1799 byte
  -> ListBuffer
3	2673514	ktamido	0.00	2018-01-10 13:56	Scala	2.11.6	#7	01:88	2756 byte

*/
/* time limit
  val n = StdIn.readLine().trim.toInt
  case class Command(cmd: String, key: Option[Int])
  
  (0 until n).map{ _ =>
    val line = StdIn.readLine().trim.split(' ')
    line.toList match {
      case a::Nil if a == "deleteFirst" => deleteFirst()
      case a::Nil if a == "deleteLast" => deleteLast()
      case a::b::Nil if a == "insert" => insert(b.toInt)
      case a::b::Nil if a == "delete" => deleteKey(b.toInt)
      case _ => 
    }
  }
  //println(commandList)
  var current = MyNil.next
  while(current != MyNil){
    if(current.prev != MyNil) print(" ")
    print(current.key)
    current = current.next
  }
  println()
 */


/*  Memory Limit Exceeded
val n = StdIn.readLine().trim.toInt
case class Command(cmd: String, key: Option[Int])

val commandList = (0 until n).map{ _ =>
  val line = StdIn.readLine().trim.split(' ')
  line.toList match {
    case a::Nil => Command(a,None)
    case a::b::Nil => Command(a,Some(b.toInt))
    case _ => throw new RuntimeException
  }
}
//println(commandList)
commandList.foreach{ e =>
  e.cmd match {
    case "insert" => insert(e.key.getOrElse(0))
    case "delete" => deleteKey(e.key.getOrElse(0))
    case "deleteFirst" => deleteFirst()
    case "deleteLast" => deleteLast()
  }
}

var current = MyNil.next
while(current != MyNil){
  if(current.prev != MyNil) print(" ")
  print(current.key)
  current = current.next
}
println()
*/
