import core.Real
import linear_programming.{SimplexPrintTextSolver, DoubleNumber, SimplexTable, RealMatrix}
import linear_programming.SimplexTable._

object LinearProgrammingPlayAround {
  def dnum(p:Real,r:Real) = DoubleNumber(p,r)

  def main(args: Array[String]) {
    val minusM = dnum(-1,0)
    val aMat = RealMatrix(Array(
      Array(1,-2,1,1,0,0,0),Array(-4,1,2,0,-1,1,0),Array(-2,0,1,0,0,0,1)))
    val bVec:RealVector = Array(11,3,1)
    val cVec:MVector = Array(3,-1,-1,0,0,minusM,minusM)
    val indices = Array(3,5,6)
    val simplexTable = SimplexTable(3,7,aMat,bVec,cVec,indices)

    SimplexPrintTextSolver.solve(simplexTable)
  }

}
