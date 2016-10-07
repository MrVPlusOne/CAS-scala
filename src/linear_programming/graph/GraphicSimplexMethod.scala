package linear_programming.graph

import core.{One, Zero, Real}
import linear_programming._
import linear_programming.SimplexTable.{RealVector, MVector}

/**
 * this class use graphic simplex method to solve the maximum flow problem
 */
case class GraphicSimplexMethod(network: DirectedNetwork,source:DirectedNode,sink:DirectedNode) {
  def convertNetworkToSimplexTable():SimplexTable={
    val edges = network.allEdges.toArray
    val nodes = network.allNodes.toArray
    val variablePos = Map((0 until edges.length).map(i=>edges(i)->i):_*)
    val edgesNumber = edges.length
    val slackNumber = edges.length
    val artificialNumber = nodes.length-2
    val colNumber = edges.length+slackNumber+artificialNumber
    val rowNumber = slackNumber+artificialNumber

    val matrix = (0 until rowNumber).map(_=>(0 until colNumber).map(_=>Zero).toArray[Real]).toArray
    val bVec:RealVector = (0 until rowNumber).map(_=>Zero).toArray
    val cVec:MVector = (0 until colNumber).map(_=>SingleNumber(Zero)).toArray  // [E...ES...SA...A]

    def ithSlackPos(i:Int)=edgesNumber+i
    def ithEdgePos(i:Int) = i
    def ithArtificialPos(i:Int) = edgesNumber+slackNumber+i

    for(i<-0 until edgesNumber){
      matrix(i)(ithEdgePos(i))=One
      matrix(i)(ithSlackPos(i))=One
      bVec(i) = edges(i).weight
      if(edges(i).to==sink){
        cVec(i)= One
      }
    }
    val minusM = DoubleNumber(-1,0)
    var n=0
    for(node<- nodes if (node!=source) && (node!=sink)){
      val row = n+edgesNumber
      for(edge<-nodes(n).edgesOut){
        matrix(row)(variablePos(edge)) = One
      }
      for(edge<-nodes(n).edgesIn){
        matrix(row)(variablePos(edge))= -1
      }
      matrix(row)(ithArtificialPos(n)) = One
      bVec(row) = Zero
      cVec(ithArtificialPos(n)) = minusM

      n += 1
    }
    val aMat = RealMatrix(matrix)
    val baseIndices = (edgesNumber until colNumber).toArray
    SimplexTable(rowNumber,colNumber,aMat,bVec,cVec,baseIndices)
  }
}


