package yar.util

import org.specs2._
import org.specs2.specification.AllExpectations
import yar.tree.Leaf

class LeafListSpec extends mutable.Specification with AllExpectations {

  "Merge sorted lists" >> {
    "Merge List(1,2,3) with List(2,3,4)" >> {
      val l1 = new LeafList(List(Leaf(1),Leaf(2),Leaf(3)))
      val l2 = new LeafList(List(Leaf(2),Leaf(3),Leaf(4)))
      (l1 merge l2).list === List(Leaf(1),Leaf(2),Leaf(2),Leaf(3),Leaf(3),Leaf(4))
    }
  }

  "Sort list" >> {
    "Sort (2,4,3,1)" >> {
      List(Leaf(2), Leaf(4), Leaf(3), Leaf(1)).mergeSort === List(Leaf(1), Leaf(2), Leaf(3), Leaf(4))
    }
  }

}
