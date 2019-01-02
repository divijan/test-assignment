package yar.util

import org.specs2._
import org.specs2.specification.AllExpectations
import yar.tree.Leaf

class LeafListSpec extends mutable.Specification with AllExpectations {

  val emptyList = List.empty[Leaf]

  "Merge sorted lists" >> {
    "Merge List(1,2,3) with List(2,3,4)" >> {
      val l1 = List(Leaf(1),Leaf(2),Leaf(3))
      val l2 = List(Leaf(2),Leaf(3),Leaf(4))
      (l1 merge l2) === List(Leaf(1),Leaf(2),Leaf(2),Leaf(3),Leaf(3),Leaf(4))
    }

    "Merge empty lists" >> {
      (emptyList merge emptyList) === emptyList
    }

    "Merge empty list with a non-empty one" >> {
      (emptyList merge List(Leaf(5), Leaf(7))) === List(Leaf(5), Leaf(7))
    }

    "Merge lists with a single element" >> {
      (List(Leaf(7)) merge List(Leaf(5))) === List(Leaf(5), Leaf(7))
    }
  }

  "Sort list" >> {
    "Sort (2,4,3,1)" >> {
      List(Leaf(2), Leaf(4), Leaf(3), Leaf(1)).mergeSort === List(Leaf(1), Leaf(2), Leaf(3), Leaf(4))
    }

    "Sort an emtpy list" >> {
      emptyList.mergeSort === emptyList
    }
  }

  "Split list where total weight exceeds the limit" >> {
    "split List(1,2,3,4) at total of 4" >> {
      val list = List(Leaf(1), Leaf(2), Leaf(3), Leaf(4))
      list.splitByTotal(4) === (List(Leaf(1), Leaf(2)), List(Leaf(3), Leaf(4)))
    }

    "split List(1,2,3,4) at total of 0" >> {
      val list = List(Leaf(1), Leaf(2), Leaf(3), Leaf(4))
      list.splitByTotal(0) === (emptyList, list)
    }

    "split List(1,2,3,4) at total of 100" >> {
      val list = List(Leaf(1), Leaf(2), Leaf(3), Leaf(4))
      list.splitByTotal(100) === (list, emptyList)
    }
  }

}
