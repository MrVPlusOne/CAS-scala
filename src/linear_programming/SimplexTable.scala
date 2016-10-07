package linear_programming

import core.{Expr, Zero, Real}
import linear_programming.SimplexTable.{MVector, RealVector}


case class SimplexTable(rowNumber:Int, colNumber:Int, aMat:RealMatrix, bVec:RealVector,cVec:MVector,baseIndices:Array[Int]) {
  require(aMat.rowNumber==rowNumber && aMat.colNumber==colNumber)
  require(bVec.length==rowNumber)
  require(cVec.length==colNumber)
  require(baseIndices.length==rowNumber)
  require(baseIndices.max<colNumber)

  val sigmaVector = calcSigmaVector()
  def calcSigmaVector():MVector={
    val elements=for(c <- 0 until colNumber) yield{
      if(isBaseIndex(c)) SingleNumber(0)
      else {
        var sum:MNumber = cVec(c)
        (0 until rowNumber).foreach(r=> {
          sum -= cVec(baseIndices(r))*aMat(r,c)
        })
        sum
      } 
    }
    Array(elements:_*)
  }

  def isBaseIndex(index:Int) = baseIndices.contains(index)
  def baseIndexRow(index:Int):Int = baseIndices.indexWhere(_==index)

  def iterateTable():IterateResult = {
    val isOptimized = (0 until colNumber).forall(c=>if(isBaseIndex(c)) true else sigmaVector(c)<Zero)
    if(isOptimized){
      //Any non-basic variable's sigma equals to 0?
      def hasUniqueSolution:Boolean={
        for(c<-0 until colNumber if !isBaseIndex(c)){
          if(sigmaVector(c).equalTo(Zero)) return false
        }
        return true
      }

      val xVec = (0 until colNumber).map(c=>if(isBaseIndex(c)) bVec(baseIndexRow(c)) else Zero).toArray

      var maxZ:Expr = Zero
      for(i<-baseIndices){
        if(cVec(i).parameter!=Zero) return NoSolution
        else{
          maxZ += xVec(i)*cVec(i).remainder
        }
      }
      Optimized(xVec,maxZ.asInstanceOf[Real],hasUniqueSolution) //Return
    }else{
      val isUnbounded = (0 until colNumber).forall(c=>{
        if(sigmaVector(c) > Zero) (0 until rowNumber).forall(r=> aMat(r,c).toDouble<=0)
        else true
      })
      if(isUnbounded) Unbounded //Return
      else{
        val max = sigmaVector.max(MNumber.ordering)
        val maxIndex = sigmaVector.indexOf(max)
        // for all aik>0 calculate theta
        val thetaVec:RealVector = (0 until rowNumber).map(r=>if(aMat(r,maxIndex).toDouble>0) bVec(r)/aMat(r,maxIndex) else Real.Infinity).toArray
        val minTheta = thetaVec.min(Real.ordering)
        val thetaRow = thetaVec.indexOf(minTheta)
        NextPivot(thetaRow,maxIndex,thetaVec) //Return
      }
    }
  }
}

object SimplexTable{
  type RealVector = Array[Real]
  type MVector = Array[MNumber]

  val bigM = Real.Infinity

}

sealed abstract class IterateResult
case class NextPivot(row:Int,col:Int,thetaVec:RealVector) extends IterateResult

sealed trait SimplexResult
case class Optimized(xVec:RealVector,maxZ:Real,hasUniqueSolution:Boolean) extends IterateResult with SimplexResult{
  override def toString: String = {
    s"Optimized:\nX = < ${xVec.map(_.exprString).mkString(",")} >\nMax = ${maxZ.exprString}\n"+
      s"is Unique Solution: $hasUniqueSolution"
  }

}
object Unbounded extends IterateResult with SimplexResult
object NoSolution extends IterateResult with SimplexResult