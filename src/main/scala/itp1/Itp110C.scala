package itp1

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object Itp110C extends App {

  val buff = ListBuffer.empty[Double]
  var n = -1
  do{
    n = StdIn.readLine().toInt
    if(n != 0){
      val nums = StdIn.readLine().split(' ').map(_.toDouble)
      val ave = nums.sum / n
      
      val rsq = nums.foldLeft(0D){ (acc, p) =>
        acc + (p - ave)*(p - ave)
      }
      val result = scala.math.sqrt(rsq/n)
      buff += result
    }
    

  } while(n!=0)
  
  buff.foreach(f => println(f"$f%.8f"))
  
}

/*

OK)
100 80 54
3236.06797749979
263.6387228699747
64.7213595499958

3236.06797750
299.11697772
64.72135955

 */