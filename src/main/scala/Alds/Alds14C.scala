package Alds

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
object Alds14C extends App {
  

  val n = StdIn.readLine().trim.toInt
  
  // RuntimeError !! (Hash値が悪さしている？)
  //val result = (0 until n).foldLeft(ListBuffer.empty[String], HashSet.empty[String]){

  // List
  //val result = (0 until n).foldLeft(List.empty[String], Set.empty[String]){
  // 対して変わらない（むしろListBufferの方が若干早い）
  //val result = (0 until n).foldLeft(ArrayBuffer.empty[String], Set.empty[String]){
  
  val result = (0 until n).foldLeft(ListBuffer.empty[String], Set.empty[String]){
  
      case ((acc, dict), _) =>
    val cmd = StdIn.readLine().split(' ')
    if(cmd(0) == "insert"){
      (acc, dict + cmd(1))
    } else {
      if(dict.contains(cmd(1))) {
        // Buffer 
        (acc += "yes", dict)
        // List ("yes" :: acc, dict)
      } else {
        // Buffer 
        (acc += "no", dict)
        // List ("no" :: acc, dict)
      }
    }
  }
  // Buffer
  println(result._1.mkString(f"%n"))
  
  // List
  // println(result._1.reverse.mkString(f"%n"))

}

/*
  // 16,777,216

  // Outofbounds minus
  //val m = max
  val m = 1046527
  val arrayMax = 1000000
  
  //def ctoi(data: String) = data.foldLeft(0)((acc, e) => acc+e)
  def ctoi(data: String) = data.zipWithIndex.foldLeft(0){ (acc, e) =>
    acc + e._2 * (if(e._1 == 'A') 1 else if(e._1 == 'C') 2 else if(e._1 == 'G') 3 else 4)
  }
  
  // A: 65, C:67, G:71, T:84
  def makeHash1(key: Int) = {
    key % m
  }

  def makeHash2(key: Int) = {
    1 + key % (m - 1)
  }
  
  //def makeHash(key: Int, i: Int) = (makeHash1(key) + i * makeHash2(key)) % m
  //def makeHash(key: Int, i: Int) = makeHash1(key) + i
  
  val n = StdIn.readLine().trim.toInt
  
  val dict = Array.fill(arrayMax + 10)("")
  
  def insert(data: String): Unit = {
    var i = 0
    //val key = ctoi(data)
    var colision = true
    while(colision){
      //val j = makeHash(key, i)
      val j = (data.hashCode.abs + i) % arrayMax
      if(dict(j) == "") {
        dict(j) = data
        colision = false
      } else if(dict(j) == data){
        colision = false
      }
      else {
        i = i + 1
      } 
    }
  }

  def search(data: String): Int = {
    //val key = ctoi(data)
    var i = (data.hashCode.abs) % arrayMax
    var ret = -1
    while(ret < 0 && i < arrayMax){
      if(dict(i) == data) {
        ret = i
      }
      i += 1
    }
    ret
  }

  val result = (0 until n).foldLeft(List.empty[String]){ (acc, _) =>
    val cmd = StdIn.readLine().split(' ')
    if(cmd(0) == "insert"){
      insert(cmd(1))
      acc
    } else {
      if(search(cmd(1)) >= 0) {
        "yes"::acc
      } else {
        "no"::acc
      }
    }
  }
  println(result.reverse.mkString(f"%n"))

 */

/* time limit 
val m = max
def makeHash(key: Int, i: Int): Int = {
  val k2 = (key / m) * 100000
  val k1 = key % m
  k2 + k1 + i
}*/

/*
val result = (0 until n).map{ _ =>
  val cmd = StdIn.readLine().split(' ')
  if(cmd(0) == "insert"){
    insert(cmd(1))
    ""
  } else {
    if(search(cmd(1)) >= 0) f"yes%n" else f"no%n"
  }
}.filterNot( p => p == "")

print(result.mkString(""))
*/

