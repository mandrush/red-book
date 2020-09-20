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
    case Branch(l, r) => if(size(l) > size(r)) 1 + depth(l) else 1 + depth(r)
    case Leaf(_) => 1
  }

  def map[A, B](tree: MyTree[A])(f: A => B): MyTree[B] = tree match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(value) => Leaf(f(value))
  }





}

object TestTree extends App {
  import MyTree._
  println(s"Size of tree: ${size(Branch(Leaf(1), Leaf(2)))}")

  println(s"Maximum of tree: ${maximum(
    Branch(
      Branch(
        Branch(Leaf(2), Leaf(23)),
        Leaf(4)
      ),
      Leaf(2)
    )
  )
  }")


  println(s"depth of tree: ${depth(
    Branch(
      Branch(
        Branch(Leaf(2), Leaf(23)),
        Leaf(4)
      ),
      Leaf(2)
    )
  )
  }")

  println(s"map of tree: add 1 to each ${map(
    Branch(
      Branch(
        Branch(Leaf(2), Leaf(23)),
        Leaf(4)
      ),
      Leaf(2)
    )
  )(_ + 1)
  }")







}

