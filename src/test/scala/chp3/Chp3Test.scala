/*
 * Copyright(c) 2017 Schibsted Media Group. All rights reserved.
 */
package chp3

import chp3.fpinscala.datastructures.Chp3
import org.scalatest.{FlatSpec, Matchers}

class Chp3Test extends FlatSpec with Matchers {

  "removeFirst" should "work" in {
    Chp3.removeFirst(List(1, 2, 3)) should be (List(2, 3))
    Chp3.removeFirst(List(1)) should be (Nil)
    Chp3.removeFirst(List()) should be (Nil)
    Chp3.removeFirst(Nil) should be (Nil)
  }

  "replaceFirst" should "work" in {
    Chp3.replaceFirst(List(1, 2, 3), 0) should be (List(0, 2, 3))
    Chp3.replaceFirst(List(1), 0) should be (List(0))
    Chp3.replaceFirst(Nil, 0) should be (List(0))
  }

  "drop" should "work" in {
    Chp3.drop(List(1, 2, 3), 1) should be (List(2, 3))
    Chp3.drop(List(1, 2, 3), 3) should be (Nil)
    Chp3.drop(List(1, 2, 3), 4) should be (Nil)
    Chp3.drop(Nil, 1) should be (Nil)
  }

  "dropWhile" should "work" in {
    Chp3.dropWhile[Int](List(5, -2, 3), _ >= 0) should be (List(-2, 3))
    Chp3.dropWhile[Int](List(5, 2, 3), _ >= 0) should be (Nil)
  }

  "length" should "work" in {
    Chp3.length(List(4, 5)) should be (2)
    Chp3.length(List()) should be (0)
  }

  "sum" should "work" in {
    Chp3.sum(List(1, 2, 3)) should be (6)
    Chp3.sum(Nil) should be (0)
  }

  "product" should "work" in {
    Chp3.product(List(1, 2, 3)) should be (6)
    Chp3.product(List(1, 0, 3)) should be (0)
    Chp3.product(Nil) should be (1)
  }

  "lengthl" should "work" in {
    Chp3.lengthl(List(4, 5)) should be (2)
    Chp3.lengthl(List()) should be (0)
  }

  "reverse" should "work" in {
    Chp3.reverse(List(1, 2, 3)) should be (List(3, 2, 1))
    Chp3.reverse(Nil) should be (Nil)
  }

  "append" should "work" in {
    Chp3.append(Nil, 1) should be (List(1))
    Chp3.append(List(1, 2), 3) should be (List(1, 2, 3))
  }

  "concat" should "work" in {
    Chp3.concat(Nil, Nil) should be (Nil)
    Chp3.concat(List(1, 2, 3), Nil) should be (List(1, 2, 3))
    Chp3.concat(List(1, 2, 3), List(4, 5)) should be (List(1, 2, 3, 4, 5))
    Chp3.concat(Nil, List(4, 5)) should be (List(4, 5))
  }

  "map" should "work" in {
    Chp3.map[Int, Int](List(1, 2, 3))(_ + 1) should be (List(2, 3, 4))
  }

  "filter" should "work" in {
    Chp3.filter(List(1, 2, 3, 4))(_ % 2 == 0) should be (List(2, 4))
  }

  "filter with flatmap" should "work" in {
    Chp3.filterFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) should be (List(2, 4))
  }

  "addelements" should "work" in {
    Chp3.addElements(List(1, 2, 3), List(4, 5, 6)) should be (List(5, 7, 9))
  }

  "zipWith" should "work" in {
    Chp3.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should be (List(5, 7, 9))
  }

  "startsWith" should "work" in {
    Chp3.startsWith(List(1, 2, 3), List(1)) should be (true)
    Chp3.startsWith(List(1, 2, 3), List(1, 2)) should be (true)
    Chp3.startsWith(List(1, 2, 3), Nil) should be (true)
    Chp3.startsWith(List(1, 2, 3), List(2)) should be (false)
    Chp3.startsWith(List(1, 2, 3), List(2, 3)) should be (false)
  }

  "hasSubsequence" should "work" in {
    Chp3.hasSubsequence(List(1, 2, 3, 4), List(4)) should be (true)
    Chp3.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) should be (true)
    Chp3.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be (true)
    Chp3.hasSubsequence(List(1, 2, 3, 4), List(5)) should be (false)
    Chp3.hasSubsequence(List(1, 2, 3, 4), List(3, 2)) should be (false)
    Chp3.hasSubsequence(List(1, 2, 3, 4, 1, 2, 5, 6), List(1, 2, 5)) should be (true)
  }

}
