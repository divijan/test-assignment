package yar

import yar.tree.Leaf
import scala.language.implicitConversions

package object util {
  implicit def sortedListToLeafList(sl: SortedList[Leaf]): LeafList = new LeafList(sl)
  implicit def listToSortableList[A](l: List[A])(implicit ord: Ordering[A]): SortableList[A] = new SortableList[A](l)
}
