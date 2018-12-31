package yar.tree

import yar.util._

import scala.collection.mutable

case class Node(children: List[Node], leaves: List[Leaf]) {
  lazy val totalWeight = leaves.map(_.weight).sum

  /**
    * This method sorts leaves in all the tree and carries leaves
    * that exceed the total weight limit for the node over to the next nodes
    * in a depth-first order. After the tree is traversed, exceeding leaves are discarded
    * @param w upper limit for the total weight of leaves
    * @return balanced tree stemming from this Node
    */
  def balance(w: Int): Node = {
    def balanceNode(carryOver: LeafList, node: Node): (Node, LeafList) = {
      val (underLimit, overLimit) = (new LeafList(node.leaves).mergeSort merge carryOver).splitByTotal(w)

      val (balancedChildren, childrenCarryOver) = balanceList(overLimit, node.children)
      Node(balancedChildren, underLimit.list) -> childrenCarryOver
    }

    def balanceList(carryOver: LeafList, nodes: List[Node]): (List[Node], LeafList) = nodes match {
      case h :: t => {
        val (bHead, headCarryOver) = balanceNode(carryOver, h)
        val (bTail, tailCarryOver) = balanceList(headCarryOver, t)
        (bHead :: bTail) -> tailCarryOver
      }
      case Nil => Nil -> carryOver
    }

    balanceNode(List.empty[Leaf], this)._1
  }

}

case class Leaf(weight: Int)
