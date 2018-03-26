package Alds

import scala.io.StdIn

object Alds13B extends App {

  var head = 0
  var tail = 0
  
  val Array(n, q) = StdIn.readLine().trim.split(' ').map(_.toInt)
  val max  = n + 1 
  val myqueue: Array[(String, Int)] = new Array[(String, Int)](max)

  //case class ProceesInfo(name: String, time: Int) 
  
  def isEmpty() = head == tail
  
  def isFull() = head == ((tail + 1) % max)
  
  def enqueue(p: (String, Int)) = {
    if(isFull()) throw new RuntimeException
    else {
      myqueue(tail) = p
      if(tail + 1 == max){
        tail = 0
      } else {
        tail += 1
      }
    }
  }
  
  def dequeue(): (String, Int) = {
    if(head == tail) throw new RuntimeException
    else {
      val ret = myqueue(head)
      if(head + 1 == max){
        head = 0
      } else {
        head += 1
      }
      ret
    }
  }
  
  (0 until n).foreach{ _ =>
    val Array(ename, etime) = StdIn.readLine().trim.split(' ')
    enqueue((ename, etime.toInt))
  }
  var executeTime: Int = 0
  while(head != tail){
    val (cname, ctime) = dequeue()
    val remaintime = if((ctime - q) < 0) 0 else ctime - q
    
    executeTime  += (if((ctime - q) < 0) ctime else q)
    
    if(remaintime == 0){
      println(cname + " " + executeTime)
    } else {
      enqueue((cname, remaintime))
    }
  }
  
}
