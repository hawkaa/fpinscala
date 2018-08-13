/*
 * Copyright(c) 2017 Schibsted Media Group. All rights reserved.
 */
package chp4

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, Some => _, None => _, Left => _, Right => _, _}


sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

}
