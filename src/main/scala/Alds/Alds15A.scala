package Alds

object Alds15A extends App {

  val sc = new java.util.Scanner(System.in)
  val n = sc.nextInt 
  val a = Array.fill(n)(sc.nextInt) 
  val q = sc.nextInt 
  val m = Array.fill(q)(sc.nextInt)
  
  def solve(i: Int, m: Int): Boolean = if(m == 0) true
    else if(i >= n) false
    else solve(i+1, m) || solve(i+1, m - a(i))
  
  val result = m.map(p => if(solve(0, p)) "yes" else "no")

  print(result.mkString(f"%n") + f"%n")

}
