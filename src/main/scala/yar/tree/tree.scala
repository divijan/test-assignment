package yar.tree

import yar.util._

import scala.collection.mutable

case class Node(children: List[Node], leaves: List[Leaf]) {
  lazy val totalWeight = leaves.map(_.weight).sum

  def balance(w: Int): Node = { //w for upper limit for the total weight of leaves
    
  }

  private def balanceThis(carryOver: List[Leaf], weightLimit: Int): Node {


  }
}

case class Leaf(weight: Int)
