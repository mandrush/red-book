package chapter3

sealed trait MyTree[+A]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

  def size[A](tree: MyTree[A]): Int = tree match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def maximum(tree: MyTree[Int]): Int = tree match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(value) => value
  }

  def depth[A](tree: MyTree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: MyTree[A])(f: A => B): MyTree[B] = tree match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(value) => Leaf(f(value))
  }

  def fold[A,B](t: MyTree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: MyTree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)
  def depthViaFold[A](t: MyTree[A]): Int = fold(t)(_ => 0)((l, r) => 1 + (l max r))
  def maximumViaFold(t: MyTree[Int]): Int = fold(t)(identity)(_ max _)

  def mapViaFold[A, B](t: MyTree[A])(f: A => B): MyTree[B] = fold(t)(a => Leaf(f(a)): MyTree[B])(Branch(_, _))

}

object TestTree extends App {
  import MyTree._
  println(s"Size of tree: ${size(Branch(Leaf(1), Leaf(2)))}")

  val testTree = Branch(
    Branch(
      Branch(Leaf(2), Leaf(23)),
      Leaf(4)
    ),
    Leaf(2)
  )

  println(s"Maximum of tree: ${maximum(testTree)}")

  println(s"depth of tree: ${depth(testTree)}")

  println(s"map of tree: add 1 to each ${map(testTree)(_ + 1)}")

  println(s"Size of tree using fold : ${fold(Branch(Leaf(1), Leaf(2)))(_ => 1)(_ + _ + 1)}")
  println(s"Depth of tree using fold : ${fold(testTree)(_ => 0)((l, r) => 1 + (l max r))}")
  println(s"Maximum of tree using fold : ${fold(testTree)(identity)(_ max _)}")

}

