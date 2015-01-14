package fp


object Chapter2 {

  // 2.1 with tailrec
  def fib(n: Int): Int = ???


  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = ???



  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)


  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)


  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))



}
