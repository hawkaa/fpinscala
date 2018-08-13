/*
 * Copyright(c) 2017 Schibsted Media Group. All rights reserved.
 */
package chp3

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {
  "size" should "work" in {
    Tree.size(Leaf(1)) should be (1)
    Tree.size(Branch(Leaf(1), Leaf(2))) should be (2)
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be (3)
  }

  "depth" should "work" in {
    Tree.depth(Leaf(1)) should be (1)
    Tree.depth(Branch(Leaf(1), Leaf(2))) should be (2)
    Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be (3)
  }

  "sizeFold" should "work" in {
    Tree.sizeFold(Leaf(1)) should be (1)
    Tree.sizeFold(Branch(Leaf(1), Leaf(2))) should be (2)
    Tree.sizeFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) should be (3)
  }

  "map" should "work" in {
    Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ + 1) should be (Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
  }



}
