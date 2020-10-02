package chapter4

import scala.{Option => _, Either => _}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case None => None
  }

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object OptionChapter extends App {
  def lift[A, B](f: A => B): Option[A] => Option[B] = x => x.map(f)

  def Try[A](a: => A): Option[A] = try Some(a) catch {
    case _: Exception => None
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      oa <- a
      ob <- b
    } yield f(oa, ob)
  }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    xs.foldRight(None: Option[List[A]])((curr, z) => map2(curr, z)((opt, list) => opt :: list))

  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] = xs match {
    case Nil    => None
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequenceUsingTraverse[A](xs: List[Option[A]]): Option[List[A]] =
    traverse(xs)(a => a)

}