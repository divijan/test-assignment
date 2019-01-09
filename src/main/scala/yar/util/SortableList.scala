package yar.util

class SortableList[A](inner: List[A])(implicit ord: Ordering[A]) {
  def sort: SortedList[A] = SortedList.mergeSort(inner)
}
