package chapter3

sealed trait MyList[+A]

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]


object MyList {
  def apply[A](xs: A*): MyList[A] = {
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))
  }

  def tail[A](xs: MyList[A]): MyList[A] = xs match {
    case Cons(_, t) => t
    case Nil => Nil //or?
  }

  def setHead[A](newHead: A, xs: MyList[A]): MyList[A] = xs match {
    case Cons(_, t) => Cons(newHead, t)
    case Nil => sys.error("Set head cannot be called on an empty list")
  }

  def drop[A](xs: MyList[A], n: Int): MyList[A] = {
    @scala.annotation.tailrec
    def go(dropped: MyList[A], iteration: Int = 0): MyList[A] = {
      if (iteration == n) dropped
      else go(tail(dropped), iteration + 1)
    }

    go(xs)
  }


  @scala.annotation.tailrec
  def dropWhile[A](xs: MyList[A])(predicate: A => Boolean): MyList[A] = xs match {
    case Cons(h, t) if predicate(h) => dropWhile(t)(predicate)
    case _ => xs
  }

  def init[A](xs: MyList[A]): MyList[A] = xs match {
    case Nil          => sys.error("Init cant be called on an empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A, B](xs: MyList[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil        => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def productWithHalt(xs: MyList[Double]): Double = xs match {
    case Cons(h, _) if h == 0.0 => 0.0
    case Cons(h, t)             => ???
      //can't be done probably
  }

  def length[A](xs: MyList[A]): Int = {
    @scala.annotation.tailrec
    def go(curr: MyList[A], length: Int): Int = curr match {
      case Cons(h, Nil) => length + 1
      case Cons(h, t)   => go(t, length + 1)
      case Nil          => 0
    }
    go(xs, 0)
  }

  def foldLeft[A, B](xs: MyList[A], z: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def go(l: MyList[A], acc: B): B = l match {
      case Nil        => acc
      case Cons(h, t) => go(t, f(acc, h))
    }
    go(xs, z)
  }

  def sumFoldLeft(xs: MyList[Int]): Int = foldLeft(xs, 0)(_ + _)
  def productFoldLeft(xs: MyList[Double]): Double = foldLeft(xs, 1.0)(_ * _)
  def lengthFoldLeft[A](xs: MyList[A]): Int = foldLeft(xs, 0){ case (acc, _) => acc + 1 }

  def reverse[A](xs: MyList[A]): MyList[A] = foldLeft(xs, Nil: MyList[A]){ case (acc, h) => Cons(h, acc) }

  def append[A](xs: MyList[A], aps: MyList[A]): MyList[A] = foldRight(xs, aps)(Cons(_, _))

  def concat[A](xss: MyList[MyList[A]]): MyList[A] = foldRight(xss, Nil: MyList[A])(append)

  def add1ToEach(xs: MyList[Int]): MyList[Int] = foldRight(xs, Nil: MyList[Int]){ case (elem, acc) => Cons(elem + 1, acc) }
  def eachDoubleToString(xs: MyList[Double]): MyList[String] = foldRight(xs, Nil: MyList[String]){ case (elem, acc) => Cons(elem.toString, acc) }

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] =
    foldRight(as, Nil: MyList[B])((h, t) => Cons(f(h), t))
  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(as, Nil: MyList[A])((h, t) => if (f(h)) Cons(h, t) else t)
  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] =
    foldRight(as, Nil: MyList[B])((h, acc) => append(f(h), acc))

  def filterUsingFlatMap[A](xs: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(xs)(h => if (f(h)) MyList(h) else Nil)

  def sumLists(first: MyList[Int], second: MyList[Int]): MyList[Int] = first match {
    case Cons(f, ft) => second match {
      case Nil => Nil
      case Cons(s, Nil) => Cons(f + s, Nil)
      case Cons(s, st) => Cons(f + s, sumLists(ft, st))
    }
  }

  def zipWith[A, B, C](as: MyList[A], bs: MyList[B])(f: (A, B) => C): MyList[C] = as match {
    case Cons(a, at) => bs match {
      case Nil => Nil
      case Cons(b, Nil) => Cons(f(a, b), Nil)
      case Cons(b, bt) => Cons(f(a, b), zipWith(at, bt)(f))
    }
  }

}

object Test extends App {

  import MyList._

  val test = MyList(1, 2, 3, 4, 5, 6, 7)
  println(tail(test))

  println(setHead(12, test))

  println(drop(test, 2))
  println(drop(test, 8)) // this ofc depends on how .tail behaves when the list is Nil

  println(dropWhile(test)(_ < 5))

  println(init(test))

  println(foldRight(MyList(1,2,3), Nil: MyList[Int])(Cons(_, _)))

  println(length(test))
  println(length(MyList(1)))
  println(length(Nil))

  println(foldLeft(test, 0)(_ + _))
  println(foldLeft(test, 1.0)(_ * _))
  println(s"Length using foldLeft: ${lengthFoldLeft(test)}")

  println(s"Reverse using fold left: ${reverse(test)}")

  println(s"Append using foldRight: ${append(test, MyList(8))}")

  println(s"Concat ${concat(MyList(MyList(1,2,3), MyList(4, 5, 6)))}")

  println(s"Add 1 to each: ${add1ToEach(test)}")
  println(s"Each double to string: ${eachDoubleToString(MyList(1.0d, 2.0d, 3.0d))}")

  println(s"3.15 using map [add 1 to each] : ${map(test)(_ + 1)}")
  println(s"3.16 using map [double to string] : ${map(MyList(1.0d, 2.0d, 3.0d))(_.toString)}")

  println(s"Filter out odd numbers from test list: ${filter(test)(_ % 2 != 0)}")

  println(s"flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3): ${flatMap(test)(i => MyList(i, i))}")

  println(s"Filter out odds using filter implemented with flatMap: ${filterUsingFlatMap(test)(_ % 2 != 0)}")

  println(s"Sum ints in two lists: ${sumLists(MyList(1,2,3,12), MyList(4,5,6,123))}")









}