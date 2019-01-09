package yar.util

case class SortedList[A] private(list: List[A])(implicit val ord: Ordering[A]) {
  import Ordering.Implicits.infixOrderingOps

  //SortedLists to be merged must have equal orderings. How to enforce that on a type system level?
  def merge(that: SortedList[A]): SortedList[A] =
    if (this.ord != that.ord) {
      throw new IllegalArgumentException("Cannot merge lists with different orderings")
    } else {
      (this.list, that.list) match {
        case (Nil, _) => that
        case (_, Nil) => this
        case (hl :: tl, hr :: tr) =>
          val (l1, l2) = list.span(_ <= hr)
          (l1, l2) match {
            case (Nil, _) => that merge this
            case (_, Nil) => new SortedList(this.list ++ that.list)
            case (_, _) => new SortedList(l1 ++ (new SortedList(l2) merge that).list)
          }
      }
    }

  def +:(a: A): SortedList[A] = new SortedList(a :: Nil) merge this
}


object SortedList {
  import Ordering.Implicits.infixOrderingOps

  def mergeSort[A](list: List[A])(implicit ord: Ordering[A]): SortedList[A] = if (list.isEmpty || list.tail.isEmpty) {
      new SortedList(list)
    } else {
      val head = list.head
      if (head <= list.tail.head) {
        var currentMax = head
        val (l1, l2) = list.tail.span(l => {
          val cm = currentMax
          currentMax = l
          l >= cm
        })
        new SortedList(head :: l1) merge mergeSort(l2)
      } else {
        var currentMin = head
        val (l1, l2) = list.tail.span(l => {
          val cm = currentMin
          currentMin = l
          l <= cm // < would make sorting stable
        })
        new SortedList((head :: l1).reverse) merge mergeSort(l2)
      }
    }

  def apply[A](as: A*)(implicit ord: Ordering[A]): SortedList[A] = mergeSort(as.toList)

  //override this default apply() to prevent users from constructing SortedList from unsorted List bypassing the check
  //can't 'override' this literally
  def apply[A](as: List[A])(implicit ord: Ordering[A]): SortedList[A] = mergeSort(as)
}

