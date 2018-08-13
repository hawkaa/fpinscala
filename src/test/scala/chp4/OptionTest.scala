/*
 * Copyright(c) 2017 Schibsted Media Group. All rights reserved.
 */
package chp4

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try
import scala.{Either => _, None => _, Option => _, Some => _, _}


class OptionTest extends FlatSpec with Matchers {
  "map" should "work" in {
    Some(5).map(_ + 1) should be (Some(6))
    None.map(a => 5) should be (None)
  }

  "getOrElse" should "work" in {
    Some(5).getOrElse(6) should be (5)
    None.getOrElse(6) should be (6)
  }

  "flatMap" should "work" in {
    Some(5).flatMap(a => Some(a + 1)) should be (Some(6))
  }

  "map2" should "work" in {
    Option.map2(Some(5), Some(6))(_ + _) should be (Some(11))
    Option.map2(Some(5), None)(_ + _) should be (None)
  }

  "sequence" should "work" in {
    Option.sequence(List(Some(1), Some(2))) should be (Some(List(1, 2)))
    Option.sequence(List(Some(1), None)) should be (None)
  }

  "traverse" should "work" in {
    Option.traverse(List("1", "2"))(a => Option.Try(a.toInt)) should be (Some(List(1, 2)))
    Option.traverse(List("1", "hello"))(a => Option.Try(a.toInt)) should be (None)
  }
}
