package com.nigelhp.validation

import scala.Function.const
import scala.util.Try

/**
  * A validation is either a Success or a Failure.
  *
  * A validation is similar to Either, but a Failure can accumulate multiple values (which is useful when
  * composing validations).
  *
  * @tparam E the type of a Failure value
  * @tparam A the type of the Success value
  */
sealed trait Validation[+E, +A]

case class Success[A](a: A) extends Validation[Nothing, A]
case class Failure[E](head: E, tail: List[E] = Nil) extends Validation[E, Nothing]

object Validation {
  def fold[E, A, B](validation: Validation[E, A])(onFailure: List[E] => B, onSuccess: A => B): B =
    validation match {
      case Failure(h, t) => onFailure(h :: t)
      case Success(a) => onSuccess(a)
    }

  def map[E, A, B](validation: Validation[E, A])(f: A => B): Validation[E, B] =
    flatMap(validation)(a => Success(f(a)))

  def flatMap[E, A, B](validation: Validation[E, A])(f: A => Validation[E, B]): Validation[E, B] =
    validation match {
      case e@Failure(_, _) => e.asInstanceOf[Validation[E, B]]
      case Success(a) => f(a)
    }

  def map2[E, A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
    (va, vb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta ++: hb +: tb)
      case (Failure(h, t), _) => Failure(h, t)
      case (_, Failure(h, t)) => Failure(h, t)
    }

  // TODO cleanup
  def map3[E, A, B, C, D](va: Validation[E, A], vb: Validation[E, B], vc: Validation[E, C])(f: (A, B, C) => D): Validation[E, D] = {
    val x: A => B => C => D = f.curried
    val y = Success(x)

    val aaa: Validation[E, B => C => D] = map2(y, va) { (z, a) =>
      z(a)
    }

    val bbb: Validation[E, C => D] = map2(aaa, vb) { (z, b) =>
      z(b)
    }

    val ccc: Validation[E, D] = map2(bbb, vc) { (z, c) =>
      z(c)
    }

    ccc
  }

  //def unit[A](a: => A): F[A]

//  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
//    map2(fab, fa) { (f, a) =>
//      f(a)
//    }
//  }

  def fromTry[A](aTry: Try[A]): Validation[Throwable, A] =
    fromTry(aTry, identity)

  def fromTry[E, A](aTry: Try[A], onFailure: Throwable => E): Validation[E, A] =
    aTry.fold(cause => Failure(onFailure(cause)), Success(_))

  def fromEither[E, A](either: Either[E, A]) =
    either.fold(Failure(_), Success(_))

  def toEither[E, A](validation: Validation[E, A]): Either[List[E], A] =
    fold(validation)(Left(_), Right(_))

  def toOption[E, A](validation: Validation[E, A]): Option[A] =
    fold(validation)(const(None), Some(_))
}