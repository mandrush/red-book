import scala.annotation.tailrec

object Chapter2 extends App {

  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(as: Array[A], currIdx: Int): Boolean = {
      if ((as.length - 1) == currIdx) true
      else if (ordered(as(currIdx), as(currIdx + 1))) go(as.tail, currIdx + 1)
      else false
    }
    go(arr, 0)
  }

  println(isSorted(Array(1,2,3), (x: Int, y: Int) => x > y))
  println(isSorted(Array(1,2,3), (x: Int, y: Int) => x < y))
  println(isSorted(Array(0,0,0), (x: Int, y: Int) => x < y))
  println(isSorted(Array(0,0,0), (x: Int, y: Int) => x >= y))

  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a)
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
