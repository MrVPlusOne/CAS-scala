package linear_programming.graph


import core.Real

import scala.collection.mutable

/**
 * Created by weijiayi on 3/26/15.
 */
class DirectedNode {
  type E = WeightedDirEdge
  private var _edgesOut = new mutable.MutableList[E]()
  private var _edgesIn = new mutable.MutableList[E]()
  def edgesOut:Seq[E] = _edgesOut
  def edgesIn:Seq[E] = _edgesIn
  def addEdge(directedEdge: E): Unit ={
    if(directedEdge.from==this){
      _edgesOut.+=(directedEdge)
    }else if(directedEdge.to==this){
      _edgesIn.+=(directedEdge)
    }else{
      throw new IllegalArgumentException("wrong edge associated!")
    }
  }
}

case class WeightedDirEdge (from:DirectedNode,to:DirectedNode, weight:Real)


class DirectedNetwork{
  type N = DirectedNode
  type E = WeightedDirEdge
  private var nodes = new mutable.MutableList[N]()
  private var edges = new mutable.MutableList[E]()
  def allNodes:Seq[N] = nodes
  def allEdges:Seq[E] = edges
  def addNode(directedNode: N):Unit={
    nodes.+=(directedNode)
  }
  def buildEdge(edge: E): Unit ={
    edge.from.addEdge(edge)
    edge.to.addEdge(edge)
    edges += edge
  }
}

object DirectedNetwork{
}