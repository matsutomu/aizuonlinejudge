package itp1

import scala.io.StdIn

object Itp111D extends App {

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
    
  }
  
  val n = StdIn.readLine().toInt

  val dices = (0 until n).map { i =>
    val nums = StdIn.readLine().split(' ').map(_.toInt)
    val dice1 = Dice(nums(0), nums(1), nums(2), nums(3), nums(4), nums(5))
    // for duplicate numbers
    val numsGrouped = nums.groupBy(n => n)
    if(numsGrouped.keys.size == 2 && numsGrouped.count(p => p._2.length == 1) >= 1) {
      val t = numsGrouped.filter(p => p._2.length == 1).head
      val k = t._1
      dice1.rollToNum(k)
    } else {
      dice1
    }
  }


  var judge = false
  var i = 0
  while(!judge && i < n){
    var base = dices(i)
    var j = i+1
    while(!judge && j < n){
      var comp = dices(j)
      // search top
      var count = 0
      comp = comp.rollToNum(base.su1)
      if(base.su1 == comp.su1){
        // roll south
        base = base.roll('S')
        comp = comp.roll('S')

        count = 0
        // 2 is match / again search top, side 3 
        while(base.su1 != comp.su1 &&
          base.su3 != comp.su3 && count < 4){
          comp = comp.roll('W')
          count += 1
        }
        
        //println("base:" + base)
        //println("comp:" + comp)
        judge = base == comp
      }
      j += 1
    }
    i += 1
  }
  
  println(if(judge) "No" else "Yes")
  
}

/*

 */