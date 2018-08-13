/*
 * Copyright(c) 2017 Schibsted Media Group. All rights reserved.
 */
package chp4

import org.scalatest.{FlatSpec, Matchers}

import scala.{Option => _, Either => _, Some => _, None => _, Left => _, Right => _, _}


class EitherTest extends FlatSpec with Matchers {
  "map" should "work" in {
    Right(5).map(_+ 1) should be (Right(6))
    val pokute: Either[String, Int] = Left("pokute")
    pokute.map(_ + 1) should be (Left("pokute"))
  }


  "flatMap" should "work" in {
    Right(5).flatMap(a => Right(a + 1)) should be (Right(6))
    val pokute: Either[String, Int] = Left("pokute")
    pokute.flatMap(a => Right(a + 1)) should be (Left("pokute"))
    Right(5).flatMap(a => pokute) should be (Left("pokute"))
  }

}
