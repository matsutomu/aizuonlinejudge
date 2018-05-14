package Dsl

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Dsl2C extends App {


  case class Node(var location: Int, var p: Int = -1, var left: Int = -1, var right: Int = -1)

  case class Point(id: Int, x: Int, y: Int)

  val NIL = -1
  var np = 0
  var points = Array.fill[Point](1000000)(Point(0, 0, 0))
  val tree = Array.fill[Node](1000000)(Node(0))


  def makeKDTree(left: Int, right: Int, depth: Int): Int = {
    if (left >= right) {
      NIL
    } else {
      val mid = (left + right) / 2
      val t = np
      np += 1
      if (depth % 2 == 0) {
        points.slice(left, right).sortWith((p1, p2) => p1.x < p2.x).copyToArray(points, left, right - left)
      } else {
        points.slice(left, right).sortWith((p1, p2) => p1.y < p2.y).copyToArray(points, left, right - left)
      }

      tree(t).location = mid
      tree(t).left = makeKDTree(left, mid, depth + 1)
      tree(t).right = makeKDTree(mid + 1, right, depth + 1)

      t
    }
  }

  def find(v: Int, sx: Int, tx: Int, sy: Int, ty: Int, depth: Int, ans: ArrayBuffer[Point]): ArrayBuffer[Point] = {
    var temp = ans
    val x = points(tree(v).location).x
    val y = points(tree(v).location).y

    if (sx <= x && x <= tx && sy <= y && y <= ty) {
      temp += points(tree(v).location)
    }

    if (depth % 2 == 0) {
      if (tree(v).left != NIL) {
        temp = if (sx <= x) find(tree(v).left, sx, tx, sy, ty, depth + 1, temp) else temp
      }
      if (tree(v).right != NIL) {
        temp = if (x <= tx) find(tree(v).right, sx, tx, sy, ty, depth + 1, temp) else temp
      }
      temp
    } else {
      if (tree(v).left != NIL) {
        temp = if (sy <= y) find(tree(v).left, sx, tx, sy, ty, depth + 1, temp) else temp
      }
      if (tree(v).right != NIL) {
        temp = if (y <= ty) find(tree(v).right, sx, tx, sy, ty, depth + 1, temp) else temp
      }
      temp
    }
  }

  val n = StdIn.readLine().toInt

  (0 until n).foreach { i =>
    val Array(x, y) = StdIn.readLine().split(' ').map(_.toInt)
    points(i) = Point(i, x, y)
    tree(i) = Node(0)
  }

  np = 0

  val root = makeKDTree(0, n, 0)

  val q = StdIn.readLine().toInt

  (0 until q).foreach { i =>
    val Array(sx, tx, sy, ty) = StdIn.readLine().split(' ').map(_.toInt)
    val ans = find(root, sx, tx, sy, ty, 0, ArrayBuffer.empty[Point])
    ans.sortWith((p1, p2) => p1.id < p2.id).foreach(p => println(p.id))

    println("")

  }

}
