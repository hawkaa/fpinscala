/*
 * Copyright(c) 2017 Schibsted Media Group. All rights reserved.
 */
package chp5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) =>  go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List[A]()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n == 0) Empty else Stream.cons(h(), t().take(n - 1))
    case Empty => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n == 0) this else t().drop(n - 1)
    case _ => Empty
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (!f(h())) Empty else Stream.cons(h(), t().takeWhile(f))
    case Empty => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Empty)((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t) =>
      if (f(h)) Stream.cons(h, t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => Stream.cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => f(h) append t)


  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (Cons(h, t), i) if i == 0 => None
    case (Cons(h, t), i) => Some(h(), (t(), i - 1))
    case _ => None
  }

  // Copy paste
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  // Copy paste
  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A], t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    case (h, h2) => h == h2
  }

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Cons(h, t) => Some(Cons(h, t), t())
    case _ => None
  }.append(Stream(Stream.empty))


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(i: Int): Stream[Int] = cons(i, from(i + 1))

  def fromUnfold(i: Int): Stream[Int] = unfold(i)(t => Some(t, t + 1))

  def fibs: Stream[Int] = {
    def go(i: Int, j: Int): Stream[Int] = {
      cons(i, go(j, i + j))
    }
    go(0, 1)
  }

  def fibsUnfold: Stream[Int] = unfold((0, 1))(t => Some(t._1, (t._2, t._1 + t._2)))

  def unfold[A, S](baseValue: S)(next: S => Option[(A, S)]): Stream[A] = {
    next(baseValue) match {
      case Some(t) => cons(t._1, unfold(t._2)(next))
      case None => empty
    }
  }

}
