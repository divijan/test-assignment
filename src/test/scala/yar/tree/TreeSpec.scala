package yar.tree

import org.specs2._
import org.specs2.specification.AllExpectations
import yar.util.LeafList

class TreeSpec extends mutable.Specification with AllExpectations {

  "Balance a tree" >> {
      val tree = Node(List(Node(), Node(), Node()), List(Leaf(2), Leaf(4), Leaf(3), Leaf(1)))
      tree.balance(3) === Node(List(Node(Nil, List(Leaf(3))), Node(), Node()), List(Leaf(1), Leaf(2)))
  }

}