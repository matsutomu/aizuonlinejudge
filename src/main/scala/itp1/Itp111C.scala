package itp1

import scala.io.StdIn

object Itp111C extends App {

  case class Dice(su1: Int, su2: Int, su3: Int, su4: Int, su5: Int, su6: Int){
    private val nums = Map(1 -> su1, 2 -> su2, 3 -> su3, 4 -> su4, 5 -> su5, 6 -> su6)
    def currentTop(): Int = nums(1)
    
    def roll(d: Char): Dice = d match {
        case 'N' => Dice(su2, su6, su3, su4, su1, su5)
        case 'S' => Dice(su5, su1, su3, su4, su6, su2)
        case 'W' => Dice(su3, su2, su6, su1, su5, su4)
        case 'E' => Dice(su4, su2, su1, su6, su5, su3)
    }
    
    def rollToNum(top: Int): Dice = {
      
      var cnt = 0
      var temp = this.copy()
      while(temp.su1 != top && cnt < 4){
        temp = temp.roll('N')
        cnt += 1
      }

      cnt = 0
      while(temp.su1 != top && cnt < 4){
        temp = temp.roll('W')
        cnt += 1
      }
      if(temp.su1 == top) temp else this
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
  // for duplicate numbers
  val numsGrouped = nums.groupBy(n => n)
  dice1 = if(numsGrouped.keys.size == 2) {
    val k = numsGrouped.filter(p => p._2.length == 1).head._1
    dice1.rollToNum(k)
  } else {
    dice1
  }
  //println("dice1: " + dice1)
  val nums2 = StdIn.readLine().split(' ').map(_.toInt)
  var dice2 = Dice(nums2(0), nums2(1), nums2(2), nums2(3), nums2(4), nums2(5))
  
  // search top
  var count = 0
  dice2 = dice2.rollToNum(dice1.su1)
  //println("dice2: " + dice2)
  
  if(dice1.su1 == dice2.su1){
    // roll south
    dice1 = dice1.roll('S')
    dice2 = dice2.roll('S')

    count = 0
    // 2 is match / again search top, side 3 
    while(dice1.su1 != dice2.su1 &&
          dice1.su3 != dice2.su3 && count < 4){
      dice2 = dice2.roll('W')
      count += 1
    }
  }
  
  println(if(dice1 == dice2) "Yes" else "No")
  
}

/*

 */