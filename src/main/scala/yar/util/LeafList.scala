package yar.util

import yar.tree.Leaf

protected class LeafList(sList: SortedList[Leaf]) {
  def splitByTotal(totalWeightLimit: Int): (SortedList[Leaf], SortedList[Leaf]) = {
    var sum = 0
    val (under, over) = sList.list.span({x => sum += x.weight; sum <= totalWeightLimit})
    SortedList(under) -> SortedList(over)
  }
}
