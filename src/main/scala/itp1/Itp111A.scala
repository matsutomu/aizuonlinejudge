package itp1

import scala.io.StdIn

object Itp111A extends App {

  case class Dice(su1: Int, su2: Int, su3: Int, su4: Int, su5: Int, su6: Int){
    private val nums = Map(1 -> su1, 2 -> su2, 3 -> su3, 4 -> su4, 5 -> su5, 6 -> su6)
    def currentTop(): Int = nums(1)
    
    def roll(d: Char): Dice = d match {
        case 'N' => Dice(su2, su6, su3, su4, su1, su5)
        case 'S' => Dice(su5, su1, su3, su4, su6, su2)
        case 'W' => Dice(su3, su2, su6, su1, su5, su4)
        case 'E' => Dice(su4, su2, su1, su6, su5, su3)
    }
    
  }
  
  val nums = StdIn.readLine().split(' ').map(_.toInt)
  var dice1 = Dice(nums(0), nums(1), nums(2), nums(3), nums(4), nums(5))

  val direction = StdIn.readLine()
  direction.foreach{ d =>
    dice1 = dice1.roll(d)
  } 
  
  println(dice1.currentTop())
  
}

/*
1 2 3 4 5 6
SWNN
=> 4
 */