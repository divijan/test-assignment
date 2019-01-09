package yar.util

import org.specs2._
import org.specs2.specification.AllExpectations
import yar.tree.Leaf

class LeafListSpec extends mutable.Specification with AllExpectations {
  val emptySortedList = SortedList[Leaf](Nil)

  "Merge sorted lists" >> {
    "Merge List(1,2,3) with List(2,3,4)" >> {
      val l1 = SortedList(Leaf(1),Leaf(2),Leaf(3))
      val l2 = SortedList(Leaf(2),Leaf(3),Leaf(4))
      (l1 merge l2) === SortedList(Leaf(1),Leaf(2),Leaf(2),Leaf(3),Leaf(3),Leaf(4))
    }

    "Merge empty lists" >> {
      (emptySortedList merge emptySortedList) === emptySortedList
    }

    "Merge empty list with a non-empty one" >> {
      (emptySortedList merge SortedList(Leaf(5), Leaf(7))) === SortedList(Leaf(5), Leaf(7))
    }

    "Merge lists with a single element" >> {
      (SortedList(Leaf(7)) merge SortedList(Leaf(5))) === SortedList(Leaf(5), Leaf(7))
    }
  }

  "Sort list" >> {
    "Sort (2,4,3,1)" >> {
      List(Leaf(2), Leaf(4), Leaf(3), Leaf(1)).sort === SortedList(Leaf(1), Leaf(2), Leaf(3), Leaf(4))
    }

    "Sort an empty list" >> {
      List.empty[Leaf].sort === emptySortedList
    }

    "Sort a 1-element list" >> {
      List(Leaf(1)).sort === SortedList(Leaf(1))
    }
  }

  "Split list where total weight exceeds the limit" >> {
    val list1234 = SortedList(Leaf(1), Leaf(2), Leaf(3), Leaf(4))

    "split SortedList(1,2,3,4) at total of 4" >> {
      list1234.splitByTotal(4) === (SortedList(Leaf(1), Leaf(2)), SortedList(Leaf(3), Leaf(4)))
    }

    "split SortedList(1,2,3,4) at total of 0" >> {
      list1234.splitByTotal(0) === (emptySortedList, list1234)
    }

    "split List(1,2,3,4) at total of 100" >> {
      list1234.splitByTotal(100) === (list1234, emptySortedList)
    }
  }

}
