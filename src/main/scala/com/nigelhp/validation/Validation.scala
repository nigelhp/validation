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
    flatMap(validation)(a => unit(f(a)))

  def flatMap[E, A, B](validation: Validation[E, A])(f: A => Validation[E, B]): Validation[E, B] =
    validation match {
      case e@Failure(_, _) => e.asInstanceOf[Validation[E, B]]
      case Success(a) => f(a)
    }

  private def unit[E, A](a: => A): Validation[E, A] =
    Success(a)

  private def apply[E, A, B](vFn: Validation[E, A => B])(vA: Validation[E, A]): Validation[E, B] =
    map2(vFn, vA) { (f, a) =>
      f(a)
    }

  def map2[E, A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
    (va, vb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta ++: hb +: tb)
      case (Failure(h, t), _) => Failure(h, t)
      case (_, Failure(h, t)) => Failure(h, t)
    }

  def map3[E, A, B, C, D](va: Validation[E, A], vb: Validation[E, B], vc: Validation[E, C])(f: (A, B, C) => D): Validation[E, D] = {
    val vbcd = apply(unit(f.curried))(va)
    val vcd = apply(vbcd)(vb)
    apply(vcd)(vc)
  }

  def fromTry[A](aTry: Try[A]): Validation[Throwable, A] =
    fromTry(aTry, identity)

  def fromTry[E, A](aTry: Try[A], onFailure: Throwable => E): Validation[E, A] =
    aTry.fold(cause => Failure(onFailure(cause)), Success(_))

  def fromEither[E, A](either: Either[E, A]): Validation[E, A] =
    either.fold(Failure(_), Success(_))

  def toEither[E, A](validation: Validation[E, A]): Either[List[E], A] =
    fold(validation)(Left(_), Right(_))

  def toOption[E, A](validation: Validation[E, A]): Option[A] =
    fold(validation)(const(None), Some(_))
}