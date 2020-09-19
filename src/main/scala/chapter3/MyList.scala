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



}