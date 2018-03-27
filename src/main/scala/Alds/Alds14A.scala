package Alds

import scala.io.StdIn

object Alds14A extends App {

  val n = StdIn.readLine().trim.toInt
  val lst1 = StdIn.readLine().trim.split(' ').map(_.toInt) ++ Array(-1)
  val t = StdIn.readLine().trim.toInt
  val lst2 = StdIn.readLine().trim.split(' ').map(_.toInt)
  /*val sc = new java.util.Scanner(System.in)
  val n = sc.nextInt // 最初の数字を読み取る
  val lst1 = Array.fill(n)(sc.nextInt) ++ Array(-1) // nextIntがn回呼ばれる

  val t = sc.nextInt // 最初の数字を読み取る
  val lst2 = Array.fill(t)(sc.nextInt) // nextIntがn回呼ばれる
  */
  
  val result = lst2.foldLeft(0){ (acc, e) =>
    lst1(n) = e
    var i = 0
    while(lst1(i)!=e)i+=1

    if(i == n) acc else acc + 1
  }
  
  println(result)

}
