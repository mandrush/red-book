package chapter4

import scala.{Either => _, Option => _}

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


sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(v)  => Left(v)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Chapter4 extends App {
  def lift[A, B](f: A => B): Option[A] => Option[B] = x => x.map(f)

  def Try[A](a: => A): Option[A] = try Some(a) catch {
    case _: Exception => None
  }

  def map2Option[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      oa <- a
      ob <- b
    } yield f(oa, ob)
  }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    xs.foldRight(None: Option[List[A]])((curr, z) => map2Option(curr, z)((opt, list) => opt :: list))

  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] = xs match {
    case Nil    => None
    case h :: t => map2Option(f(h), traverse(t)(f))(_ :: _)
  }

  def sequenceUsingTraverse[A](xs: List[Option[A]]): Option[List[A]] =
    traverse(xs)(a => a)

  def traverseEither[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverseEither(t)(f))(_ :: _)
  }

  def sequenceEither[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverseEither(es)(_ => _)
  }

}