package itp2

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp21C extends App {

  val buff = ListBuffer.empty[Int]
  var cursor = -1 // 0 origin

  def dump(): Unit = {
    println(s"buff: $buff")
    println(s"cursor: $cursor")
    println("---------------------")

    
  }


  def insert(x: Int): Unit = {
    if (cursor < 0) {
      buff.append(x)
      cursor = 0
    }
    else if (cursor == buff.size) {
      buff.append(x)
    }
    else {
      buff.insert(cursor, x)
      //cursor += 1
    }

    //println(s"insert $x");dump()
  }

  def move(d: Int): Unit = {
    cursor = cursor + d
    cursor = if (cursor < 0) 0
    else if (buff.size < cursor) buff.size - 1
    else cursor

    //println(s"move $d");dump()
  }

  val q = StdIn.readLine().trim.toInt
  (0 until q).foreach { _ =>
    val input = StdIn.readLine()
    val a = if (input.startsWith("2")) Array(2, 0) else input.split(' ').map(_.toInt)
    val (cmd, x): (Int, Int) = (a(0), a(1))
    cmd match {
      case 0 => insert(x)
      case 1 => move(x)
      case 2 =>
        if(0 <= cursor) buff.remove(cursor)
          //println(s"remove $cursor");dump()

        cursor = if(buff.isEmpty) 0 else if (buff.size < cursor) buff.size - 1 else cursor

    }

  }

  buff.foreach(println)


}

