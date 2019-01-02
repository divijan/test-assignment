package yar.util

import yar.tree.Leaf
import yar.util._

protected class LeafList(val list: List[Leaf]) {
  //assume this and that are sorted. Public for efficiency reasons (see yar.tree.Node:19)
  def merge(that: List[Leaf]): List[Leaf] = (this.list, that) match {
    case (Nil, _) => that
    case (_, Nil) => this.list
    case (hl :: tl, hr :: tr) => if (hl.weight <= hr.weight) {
      hl :: (tl merge that)
    } else {
      hr :: (this.list merge tr)
    }
  }


  def mergeSort: List[Leaf] = {
    val halfIndex = list.length / 2
    if (halfIndex == 0) {
      list
    } else {
      val (left, right) = list.splitAt(halfIndex)
      left.mergeSort merge right.mergeSort
    }
  }


  def splitByTotal(totalWeightLimit: Int): (List[Leaf], List[Leaf]) = {
    var sum = 0
    list.span({x => sum += x.weight; sum <= totalWeightLimit})
  }
}
