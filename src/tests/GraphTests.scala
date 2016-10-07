package tests

import core.Zero
import linear_programming.graph.{GraphicSimplexMethod, WeightedDirEdge, DirectedNode, DirectedNetwork}
import linear_programming.SimplexPrintTextSolver

/**
 * Created by weijiayi on 3/26/15.
 */
class GraphTests extends MyTest{
  "A Directed Network" should {
    val network = new DirectedNetwork()
    val List(n1, n2, n3) = (0 until 3).map(_ => new DirectedNode()).toList
    "be able to add 3 Nodes" in {
      network.addNode(n1)
      network.addNode(n2)
      network.addNode(n3)
      network.allNodes.length shouldBe 3
    }
    "be able to add Edges" in {
      val e1 = new WeightedDirEdge(n1, n2, 3)
      network.buildEdge(e1)
      assert(n1.edgesOut.contains(e1))
      assert(n2.edgesIn.contains(e1))
      network.buildEdge(new WeightedDirEdge(n1, n3, 2))
      network.buildEdge(new WeightedDirEdge(n3, n1, 1))
      n1.edgesOut.length shouldBe 2
      n2.edgesIn.length shouldBe 1
      n3.edgesIn.length shouldBe 1
    }
    "Then give me the simplex table" in {
      val table = GraphicSimplexMethod(network,source = n3,sink = n2).convertNetworkToSimplexTable()
//      SimplexPrintTextSolver.solve(table)
    }
  }
}
