package yar

import yar.tree.Leaf

package object util {
  implicit class LeafList(list: List[Leaf]) {
    lazy val totalWeight = list.map(_.weight).sum

    def mergeSort: List[Leaf] {
      ???
    }

    def splitByTotal(totalWeightLimit: Int): (LeafList, LeafList) = {
      val sorted = mergeSort

      def loop(runningTotal: Int, underLimit: Vector[Leaf], overLimit: List[Leaf]): (List[Leaf], List[Leaf]) = overLimit match {
        case h :: t => if (runningTotal + h.weight > totalWeightLimit) {
          underLimit.toList -> overLimit
        } else {
          loop(runningTotal + h.weight, underLimit :+ h, t)
        }
        case Nil => (underLimit.toList, Nil)
      }

      loop(0, Vector.empty[Leaf], sorted)
    }
  }
}
