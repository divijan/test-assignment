package yar.util

import yar.tree.Leaf
import yar.util._

class LeafList(val list: List[Leaf]) {
  lazy val totalWeight = list.map(_.weight).sum

  def isEmpty = list.isEmpty

  def ++(other: LeafList) = new LeafList(list ++ other.list)

  //assume this and that are sorted
  def merge(that: LeafList): List[Leaf] = (this.list, that.list) match {
    case (Nil, _) => that.list
    case (_, Nil) => this.list
    case (hl :: tl, hr :: tr) => if (hl.weight <= hr.weight) {
      hl :: (tl merge that.list)
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


  def splitByTotal(totalWeightLimit: Int): (LeafList, LeafList) = {
    def loop(runningTotal: Int, underLimit: Vector[Leaf], overLimit: List[Leaf]): (List[Leaf], List[Leaf]) = overLimit match {
      case h :: t => if (runningTotal + h.weight > totalWeightLimit) {
        underLimit.toList -> overLimit
      } else {
        loop(runningTotal + h.weight, underLimit :+ h, t)
      }
      case Nil => (underLimit.toList, Nil)
    }

    val (under, over) = loop(0, Vector.empty[Leaf], list)
    (listToLeafList(under), listToLeafList(over))
  }
}
