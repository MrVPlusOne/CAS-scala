package linear_programming

import core.Real
import linear_programming.RealMatrix.Row

/**
 * Created by weijiayi on 3/24/15.
 */
class GenericMatrix[T](elements:Array[Array[T]]) {
  val rowNumber = elements.length
  val colNumber = elements(0).length
  checkSize()
  def checkSize(): Unit ={
    val sameSize=elements.forall(row=>row.length==colNumber)
    require(sameSize,"Rows should have same size!")
  }
  
  def apply(row:Int,col:Int):T = elements(row)(col)
}

object RealMatrix{
  type Row = Array[Real]
}
case class RealMatrix(elements:Array[Row]) extends GenericMatrix[Real](elements){
  def getScaledRow(row:Int,scale:Real):Row = elements(row).map(scale*_)

  def addRowTo(addend:Row,toRow:Int):Row = {
    (0 until colNumber).map(c=>addend(c)+elements(toRow)(c)).toArray
  }

  override def toString: String = elements.map(row=>"\t"+row.map(_.exprString).mkString("\t")).mkString("\n")
}