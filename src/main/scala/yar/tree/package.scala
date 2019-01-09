package yar

package object tree {
  implicit val orderingLeaf = Ordering.by[Leaf, Int](_.weight)
}
