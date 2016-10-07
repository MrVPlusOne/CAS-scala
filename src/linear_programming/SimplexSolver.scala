package linear_programming

import core.{MathString, Real, Expr, One}

/**
 * Created by weijiayi on 3/25/15.
 */
abstract class SimplexSolver {

  def printTable(table: SimplexTable, pivot: NextPivot):Unit
  def printTable(table: SimplexTable)
  def printResult(simplexResult: SimplexResult):Unit

  def solve(table:SimplexTable):SimplexResult={
    table.iterateTable() match{
      case pivotInfo @ NextPivot(row,col,thetaVec)=>
        val (aMat,bVec)=(table.aMat,table.bVec)
        val invScale = One./(aMat(row,col):Expr).asInstanceOf[Real]
        val mainRow = aMat.getScaledRow(row,invScale)
        val mainB = bVec(row)*invScale
        val rowNumber: Int = table.rowNumber
        val newBVec = new Array[Real](rowNumber)
        val newRows = for(r<-0 until rowNumber) yield {
          if(r!=row){
            val minusScale = aMat(r,col)* -1
            newBVec(r)=mainB*minusScale+bVec(r)
            aMat.addRowTo(mainRow.map(_*minusScale),r)
          }else{
            newBVec(r)=mainB
            mainRow
          }
        }
        val newMat = RealMatrix(newRows.toArray)
        val newBasis = (0 until rowNumber).map(i=>if(i==row) col else table.baseIndices(i)).toArray

        printTable(table,pivotInfo)

        solve(SimplexTable(rowNumber,table.colNumber,newMat,newBVec,table.cVec,newBasis))

      case result:SimplexResult=> {
        printTable(table)
        printResult(result)
      };result
    }
  }
}

object SimplexPrintTextSolver extends SimplexSolver{
  override def printTable(table: SimplexTable, pivot: NextPivot): Unit = {
    println(s"CVec = \t${table.cVec.mkString(",\t")}")
    printTable(table)
    println(s"Pivot At (r:${pivot.row+1} c:${pivot.col+1})")
    println(s"thetas = \t<\t${pivot.thetaVec.map(expr=>MathString(expr)).mkString("\t")}\t>")
  }

  override def printResult(simplexResult: SimplexResult): Unit = {
    println("==>")
    println("       "+simplexResult)
    println("============")
  }

  override def printTable(table: SimplexTable): Unit = {
    println("------------")
    println(s"Matrix = \n${table.aMat}")
    println(s"BVec = \t<\t${table.bVec.map(_.exprString).mkString("\t")}\t>")
    println(s"Basis Indices = \t<\t${table.baseIndices.mkString("\t")}\t>")
    println(s"Sigmas = \t<\t${table.sigmaVector.mkString("\t")}\t>")
  }
}