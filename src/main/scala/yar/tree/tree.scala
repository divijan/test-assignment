package yar.tree

import yar.util._

import scala.collection.mutable

case class Node(children: List[Node], leaves: List[Leaf]) {
  /** Returns a balanced tree where each node's leaves are sorted by weight and do not exceed
    * weight limit. Leaves that exceed the total weight limit for the node are transferred over
    * to the next node in a depth-first order. After the tree is traversed, exceeding leaves are discarded.
    * @param w upper limit for the total weight of leaves
    * @return balanced tree with this Node as root
    */
  def balance(w: Int): Node = {
    def balanceNode(carryOver: List[Leaf], node: Node): (Node, List[Leaf]) = {
      val (underLimit, overLimit) = (node.leaves.mergeSort merge carryOver).splitByTotal(w)

      val (balancedChildren, childrenCarryOver) = balanceList(overLimit, node.children)
      Node(balancedChildren, underLimit.list) -> childrenCarryOver
    }

    def balanceList(carryOver: List[Leaf], nodes: List[Node]): (List[Node], List[Leaf]) = nodes match {
      case h :: t => {
        val (bHead, headCarryOver) = balanceNode(carryOver, h)
        val (bTail, tailCarryOver) = balanceList(headCarryOver, t)
        (bHead :: bTail) -> tailCarryOver
      }
      case Nil => Nil -> carryOver
    }

    balanceNode(Nil, this)._1
  }

}

object Node {
  def apply(): Node = Node(Nil, Nil)
}

case class Leaf(weight: Int) {
  //took a liberty to require a positive weight here, instead of integer non-zero, as per requirements
  require(weight > 0, "Leaf weight must be > 0")
}
