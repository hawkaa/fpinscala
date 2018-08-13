/*
 * Copyright(c) 2017 Schibsted Media Group. All rights reserved.
 */
package chp3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(a, b) => g(fold(a)(f)(g), fold(b)(f)(g))
  }
  /*
  def fold[A, B](tree: Tree[A], a: B)(f: (A, B) => B): B = {
    case Leaf(_) => a
    case Branch(left: Tree[A], right: Tree[A]) => f(fold(left, a)(f), fold(right, a)(f))
  }
  */

  def sizeFold[A](tree: Tree[A]): Int = fold(tree)(a => 1)(_ + _)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
