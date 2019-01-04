package yar.util

import yar.tree.Leaf

protected class LeafList(val list: List[Leaf]) {
  //assume this and that are sorted. Public for efficiency reasons (see yar.tree.Node:19)
  def merge(that: List[Leaf]): List[Leaf] = (this.list, that) match {
    case (Nil, _) => that
    case (_, Nil) => this.list
    case (hl :: tl, hr :: tr) =>
      val (l1, l2) = list.span(_.weight <= hr.weight)
      (l1, l2) match {
        case (Nil, _) => that merge this.list
        case (_, Nil) => this.list ++ that
        case (_, _) => l1 ++ (l2 merge that)
      }
  }


  def mergeSort: List[Leaf] = if (list.isEmpty || list.tail.isEmpty) {
      list
    } else {
      val headWeight = list.head.weight
      if (headWeight <= list.tail.head.weight) {
        var currentMax = headWeight
        val (l1, l2) = list.tail.span(l => {
          val cm = currentMax
          currentMax = l.weight
          l.weight >= cm
        })
        (list.head :: l1) merge l2.mergeSort
      } else {
        var currentMin = headWeight
        val (l1, l2) = list.tail.span(l => {
          val cm = currentMin
          currentMin = l.weight
          l.weight <= cm // < would make sorting stable
        })
        (list.head :: l1).reverse merge l2.mergeSort
      }
    }


  def splitByTotal(totalWeightLimit: Int): (List[Leaf], List[Leaf]) = {
    var sum = 0
    list.span({x => sum += x.weight; sum <= totalWeightLimit})
  }
}
