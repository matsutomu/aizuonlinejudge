package itp1

import scala.io.StdIn

object Itp111B extends App {

  case class Dice(su1: Int, su2: Int, su3: Int, su4: Int, su5: Int, su6: Int){
    private val nums = Map(1 -> su1, 2 -> su2, 3 -> su3, 4 -> su4, 5 -> su5, 6 -> su6)
    def currentTop(): Int = nums(1)
    
    def roll(d: Char): Dice = d match {
        case 'N' => Dice(su2, su6, su3, su4, su1, su5)
        case 'S' => Dice(su5, su1, su3, su4, su6, su2)
        case 'W' => Dice(su3, su2, su6, su1, su5, su4)
        case 'E' => Dice(su4, su2, su1, su6, su5, su3)
    }
    
    def dicePattern(top: Int, front: Int): Int = (top, front) match {
        case (this.su4, this.su2) => this.su1
        case (this.su2, this.su3) => this.su1
        case (this.su3, this.su5) => this.su1
        case (this.su5, this.su4) => this.su1
        case (this.su1, this.su4) => this.su2
        case (this.su4, this.su6) => this.su2
        case (this.su6, this.su3) => this.su2
        case (this.su3, this.su1) => this.su2
        case (this.su6, this.su5) => this.su3
        case (this.su5, this.su1) => this.su3
        case (this.su1, this.su2) => this.su3
        case (this.su2, this.su6) => this.su3
        case (this.su1, this.su5) => this.su4
        case (this.su5, this.su6) => this.su4
        case (this.su6, this.su2) => this.su4
        case (this.su2, this.su1) => this.su4
        case (this.su1, this.su3) => this.su5
        case (this.su3, this.su6) => this.su5
        case (this.su6, this.su4) => this.su5
        case (this.su4, this.su1) => this.su5
        case (this.su2, this.su4) => this.su6
        case (this.su4, this.su5) => this.su6
        case (this.su5, this.su3) => this.su6
        case (this.su3, this.su2) => this.su6
    }
    
  }
  
  val nums = StdIn.readLine().split(' ').map(_.toInt)
  var dice1 = Dice(nums(0), nums(1), nums(2), nums(3), nums(4), nums(5))
  val n = StdIn.readLine().toInt
  val results = (0 until n).map{ _ =>
    val Array(top, front) = StdIn.readLine().split(' ').map(_.toInt)
    dice1.dicePattern(top, front)
  }
  
  results.foreach(println)
  
}

/*

 */