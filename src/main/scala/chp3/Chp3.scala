/*
 * Copyright(c) 2017 Schibsted Media Group. All rights reserved.
 */
package chp3

package fpinscala.datastructures

import scala.annotation.tailrec

//sealed trait List[+A] // `List` data type, parameterized on a type, `A`
//case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
//case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Chp3 {
  def removeFirst[A](as: List[A]): List[A] = {
    as match {
      case x :: xs => xs
      case _ => Nil
    }
  }

  def replaceFirst[A](as: List[A], a: A): List[A] = {
    as match {
      case x :: xs => a :: xs
      case _ => List(a)
    }
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    as match {
      case x :: xs => {
        if (n == 1) xs
        else drop(xs, n - 1)
      }
      case _ => Nil
    }
  }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = {
    as match {
      case x :: xs => {
        if (!f(x)) x :: xs
        else dropWhile(xs, f)
      }
      case _ => Nil
    }
  }

  // ex 3.8
  // Maybe the List constructor is created with a fold right method

  def foldRight[A, B](l: List[A], v: B)(f: (A, B) => B): B = {
    l match {
      case x :: xs => f(x, foldRight(xs, v)(f))
      case Nil => v
    }
  }

  def length[A](l: List[A]) = foldRight(l, 0)((_, b) => b + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], b: B)(f: (B, A) => B): B = {
    as match {
      case Nil => b
      case x :: xs => foldLeft(xs, f(b, x))(f)
    }
  }

  def sum(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product(l: List[Int]) = foldLeft(l, 1)(_ * _)
  def lengthl[A](l: List[A]) = foldLeft(l, 0)((b, _) => b + 1)

  def reverse[A](l: List[A]) = foldLeft[A, List[A]](l, Nil)((as, a) => a :: as)
  def append[A](as: List[A], b: A)  = foldRight[A, List[A]](as, List(b))(_ :: _)
  def concat[A](as: List[A], bs: List[A]) = foldRight(as, bs)(_ :: _)

  def cnct[A](as: List[List[A]]) = foldRight(as, Nil:List[A])(concat[A])

  def map[A, B](as: List[A])(f: A => B) = foldRight(as, Nil:List[B])((a, as) => f(a) :: as)

  def filter[A](as: List[A])(f: A => Boolean) = foldRight(as, Nil:List[A])((a, as) => {
    if (f(a)) a :: as
    else as
  })

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    cnct(map(l)(f))

  def filterFlatMap[A](as: List[A])(f: A => Boolean) = flatMap(as)( a => if (f(a)) List(a) else Nil)

  def addElements(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => bs
    case (_, Nil) => as
    case (x :: xs, y :: ys) => (x + y) :: addElements(xs, ys)
  }

  def zipWith[A](as: List[A], bs: List[A])(f : (A, A) => A): List[A] = (as, bs) match {
    case (Nil, _) => bs
    case (_, Nil) => as
    case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (x :: xs, y :: ys) => if (x == y) startsWith(xs, ys) else false

  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (startsWith(sup, sub)) true else sup match {
      case x :: xs => hasSubsequence(xs, sub)
      case _ => false
    }
  }

}
