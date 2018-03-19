package com.nigelhp.validation

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
  def fold[E, A, B](onFailure: List[E] => B, onSuccess: A => B)(validation: Validation[E, A]): B =
    validation match {
      case Failure(h, t) => onFailure(h :: t)
      case Success(a) => onSuccess(a)
    }

  def fromTry[A](aTry: Try[A]): Validation[Throwable, A] =
    fromTry(aTry, identity)

  def fromTry[E, A](aTry: Try[A], onFailure: Throwable => E): Validation[E, A] =
    aTry.fold(cause => Failure(onFailure(cause)), Success(_))
}