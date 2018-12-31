package yar

import yar.tree.Leaf

package object util {
  implicit def listToLeafList(l: List[Leaf]): LeafList = new LeafList(l)
}
