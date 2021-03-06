package yar.tree

import org.specs2._
import org.specs2.specification.AllExpectations
import yar.util.LeafList

class TreeSpec extends mutable.Specification with AllExpectations {

  "Attempt to construct a Leaf with illegal weight should throw an exception" >> {
    "Negative weight" >> {
      Leaf(-2) must throwAn[IllegalArgumentException](message = "requirement failed: Leaf weight must be > 0")
    }

    "Zero weight" >> {
      Leaf(0) must throwAn[IllegalArgumentException](message = "requirement failed: Leaf weight must be > 0")
    }
  }

  "Balance example tree from assignment" >> {
    val tree = Node(List(Node(),
                         Node(),
                         Node()),
                    List(Leaf(2), Leaf(4), Leaf(3), Leaf(1)))
    tree.balance(3) === Node(List(Node(Nil,
                                       Leaf(3) :: Nil),
                                  Node(),
                                  Node()),
                             List(Leaf(1), Leaf(2)))
  }

  "Balance another tree" >> {
    val tree = Node(List(Node(Nil,
                              List(Leaf(2), Leaf(4), Leaf(3), Leaf(1))),
                         Node(Node() :: Nil,
                              Nil),
                         Node()),
                    Nil)
    tree.balance(4) === Node(List(Node(Nil,
                                       List(Leaf(1), Leaf(2))),
                                  Node(Node(Nil,
                                            Leaf(4) :: Nil) :: Nil,
                                       Leaf(3) :: Nil),
                                  Node()),
                             Nil)
  }

}