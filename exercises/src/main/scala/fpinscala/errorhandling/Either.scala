package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(err) => Left(err)
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case Left(err) => Left(err)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(value) => Right(value)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aRight <- this
      bRight <- b
    } yield f(aRight, bRight)
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case x :: xs => f(x).map2(traverse(xs)(f))(_ :: _)
  }

  def traverseFold[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val seed: Either[E, List[B]] = Right(Nil)
    es.foldRight(seed) {
      case (a: A, listOfB: Either[E, B]) => f(a).map2(listOfB)(_ :: _)
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def main(args: Array[String]): Unit = {

    val c = List("1", "2", "3", "4")
    val d = List("1", "2", "3", "haha")

    println(traverseFold(c)(i => Try(i.toInt)))
    println(traverseFold(d)(i => Try(i.toInt)))
  }
}