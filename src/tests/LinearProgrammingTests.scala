package tests

import core.{Rational, NumInt, Real}
import linear_programming.SimplexTable.{MVector, RealVector}
import linear_programming._

/**
 * Created by weijiayi on 3/24/15.
 */
class LinearProgrammingTests extends MyTest {
  def dnum(p:Real,r:Real) = DoubleNumber(p,r)

  "ajflksfjsk" should{

  }

  "The example SimplexTable" should{
    val aMat=RealMatrix(Array(
      Array(1,2,1,0,0), Array(4,0,0,1,0), Array(0,4,0,0,1)))
    val bVec:RealVector = Array(8,16,12)
    val cVec:MVector = Array(2,3,0,0,0).map(i=>SingleNumber(i))
    val indices = Array(2,3,4)
    val simplexTable=SimplexTable(rowNumber = 3,colNumber = 5,aMat=aMat,bVec=bVec,cVec =cVec,baseIndices = indices)
    "give right sigmas" in {
      val sigmaVec = simplexTable.sigmaVector
      val expected = Array(2,3,0,0,0).map(i=>SingleNumber(i))
      for(i<- 0 until sigmaVec.length){
        sigmaVec(i).equalTo(expected(i)) shouldBe true
      }
    }
    "give the right pivot" in {
      val result = simplexTable.iterateTable()
      val isCorrect = result match{
        case NextPivot(2,1,_)=> true
        case other => false
      }
      assert(isCorrect,s"Should not be $result")
    }
//    "print some results" in {
//      SimplexPrintTextSolver.solve(simplexTable)
//    }
  }

  "The bigM example" should{
    val minusM = dnum(-1,0)
    val aMat = RealMatrix(Array(
      Array(1,-2,1,1,0,0,0),Array(-4,1,2,0,-1,1,0),Array(-2,0,1,0,0,0,1)))
    val bVec:RealVector = Array(11,3,1)
    val cVec:MVector = Array(3,-1,-1,0,0,minusM,minusM)
    val indices = Array(3,5,6)
    val simplexTable = SimplexTable(3,7,aMat,bVec,cVec,indices)
    "give right sigmas" in {
      val sigmaVec = simplexTable.sigmaVector
      val expected:MVector = Array(dnum(-6,3),dnum(1,-1),dnum(3,-1),0,dnum(-1,0),0,0)
      for(i<- 0 until sigmaVec.length){
        assert(sigmaVec(i).equalTo(expected(i)),s"\n>>>component $i : ${sigmaVec(i)} not equal to ${expected(i)}")
      }
    }
    "give the right pivot" in {
      simplexTable.iterateTable() match{
        case NextPivot(2,2,_)=> assert(true)
        case other=>fail(s"Should not be $other")
      }
    }
//    "print some results" in {
//      SimplexPrintTextSolver.solve(simplexTable)
//    }
  }

  "MNumbers should calculate to right results" in {
    dnum(1,2)-dnum(3,4) shouldBe dnum(-2,-2)
    dnum(1,2)+dnum(1,0)-SingleNumber(4) shouldBe dnum(2,-2)
    dnum(1,0)*(-3) shouldBe dnum(-3,0)
    dnum(3,4)/4 shouldBe dnum(Rational(3,4),1)
  }
}
