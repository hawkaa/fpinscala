/*
 * Copyright(c) 2017 Schibsted Media Group. All rights reserved.
 */
package chp5

import org.scalatest.{FlatSpec, Matchers}


class StreamTest extends FlatSpec with Matchers {
  "toListRecursive" should "work" in {
    Stream(1, 2, 3).toListRecursive should equal (List(1, 2, 3))
  }

  "toList" should "work" in {
    Stream(1, 2, 3).toList should equal (List(1, 2, 3))
  }

  "take" should "work on an empty stream" in {
    Empty.take(2) should equal (Empty)
  }

  "take" should "be empty when no elements are taken" in {
    Stream(1).take(0) should equal (Empty)
  }

  "take" should "be the same when all elements are taken" in {
    Stream(1).take(1).toList should equal (List(1))
  }

  "take" should "work" in {
    Stream(1, 2, 3).take(2).toList should equal (List(1, 2))
  }

  "drop" should "work on empty streams" in {
    Empty.drop(1) should equal (Empty)
  }

  "drop" should "work when removing all elements" in {
    Stream(1, 2).drop(2) should equal (Empty)
  }

  "drop" should "work" in {
    Stream(1, 2, 3).drop(2).toList should equal (List(3))
  }

  "takeWhile" should "work" in {
    Stream(1, 2, 3).takeWhile(_ % 2 != 0).toList should equal (List(1))
  }

  "forAll" should "return true" in {
    Stream(2, 4, 6).forAll(_ % 2 == 0) should equal (true)
  }
  "forAll" should "return fale" in {
    Stream(2, 3, 6).forAll(_ % 2 == 0) should equal (false)
  }

  "map" should "work" in {
    Stream(1, 2, 3).map(_ + 1).toList should equal (List(2, 3, 4))
  }

  "constant" should "work" in {
    Stream.constant(5).take(2).toList should equal(List(5, 5))
  }

  "from" should "work" in {
    Stream.from(5).take(2).toList should equal(List(5, 6))
  }

  "fibs" should "work" in {
    Stream.fibs.take(6).toList should equal(List(0, 1, 1, 2, 3, 5))
  }

  "unfold" should "work" in {
    Stream.unfold[Int, Int](0)(f => Some((f, f + 1))).take(3).toList should equal (List(0, 1, 2))
  }

  "fromUnfold" should "work" in {
    Stream.fromUnfold(5).take(2).toList should equal(List(5, 6))
  }

  "fibsUnfold" should "work" in {
    Stream.fibsUnfold.take(6).toList should equal(List(0, 1, 1, 2, 3, 5))
  }

  "mapUnfold" should "work" in {
    Stream(1, 2, 3).mapUnfold(_ + 1).toList should equal (List(2, 3, 4))
  }

  "takeUnfold" should "work" in {
    Stream(1, 2, 3).takeUnfold(2).toList should equal (List(1, 2))
  }

  "startsWith" should "work" in {
    Stream(1, 2, 3).startsWith(Stream(1)) should be(true)
    Stream(1, 2, 3).startsWith(Stream(1, 2)) should be(true)
    Stream(1, 2, 3).startsWith(Stream.empty) should be(true)
    Stream(1, 2, 3).startsWith(Stream(2)) should be(false)
    Stream(1, 2, 3).startsWith(Stream(2, 3)) should be(false)
  }

  "tails" should "work" in {
    Stream(1, 2).tails.toList.map(_.toList) should be (List(List(1, 2), List(2), List()))
  }
}
