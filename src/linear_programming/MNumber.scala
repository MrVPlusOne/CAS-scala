package linear_programming

import core._

/**
 * Created by weijiayi on 3/25/15.
 */
sealed abstract class MNumber{

  def + (that:MNumber):MNumber
  def - (that:MNumber):MNumber
  def * (that:MNumber):MNumber
  def / (that:MNumber):MNumber
  def parameter:Real
  def remainder:Real

  def equalTo (that:MNumber,tolerance:Double = 1e-8):Boolean={
    math.abs(parameter.toDouble-that.parameter.toDouble)<tolerance &&
    math.abs(remainder.toDouble - that.remainder.toDouble) < tolerance
  }

  def <(that:MNumber):Boolean
  def <=(that:MNumber):Boolean = if(this.<(that)) true else this.equalTo(that)
  def >(that:MNumber):Boolean = ! this.<(that)

  val bigMSymb = Symb("M")
}

object MNumber{
  implicit def realToSingle(real:Real):SingleNumber = SingleNumber(real)
  implicit def intToSingle(int:Int):SingleNumber = SingleNumber(int)
  val ordering = new Ordering[MNumber]{
    override def compare(x: MNumber, y: MNumber): Int =
      if( x<y ) -1
      else if(y<x) 1
      else 0
  }
}

/**
 * a wrapper class of Real
 * @param real
 */

case class SingleNumber(real:Real) extends MNumber{

  override def toString: String = real.exprString

  def unary_- :SingleNumber = SingleNumber(real*(-1))

  override def +(that: MNumber): MNumber = that match{
    case SingleNumber(num)=>SingleNumber(real + num)
    case dNum=>dNum+this
  }

  override def /(that: MNumber): MNumber = that match{
    case SingleNumber(num)=>SingleNumber(real / num)
    case dNum=> if(that.parameter!= Zero) Zero else SingleNumber(real/that.remainder)
  }

  override def -(that: MNumber): MNumber = that match{
    case SingleNumber(num)=>SingleNumber(real - num)
    case dNum:DoubleNumber=> -dNum + this
  }

  override def *(that: MNumber): MNumber = that match{
    case SingleNumber(num)=>SingleNumber(real * num)
    case dNum=>dNum*this
  }

  override def parameter: Real = Zero

  override def remainder: Real = real

  override def <(that: MNumber): Boolean = that match{
    case SingleNumber(r) => real.toDouble<r.toDouble
    case DoubleNumber(p,r) => {
      if(p.toDouble>0) true
      else if(p.toDouble<0) false
      else real.toDouble<r.toDouble
    }
  }

}

/**
 * 
 * @param parameter the parameter of bigM
 * @param remainder the normal part
 */
case class DoubleNumber(parameter:Real,remainder:Real) extends MNumber{

  def unary_- :DoubleNumber = DoubleNumber(parameter*(-1),remainder)

  override def +(that: MNumber): MNumber = DoubleNumber(parameter+that.parameter,remainder+that.remainder)

  override def /(that: MNumber): MNumber = that match{
    case SingleNumber(real)=>DoubleNumber(parameter/real,remainder/real)
    case _=> throw new IllegalArgumentException("No bigM squared allowed")
  }

  override def -(that: MNumber): MNumber = DoubleNumber(parameter-that.parameter,remainder-that.remainder)

  override def *(that: MNumber): MNumber = that match{
    case SingleNumber(real)=>DoubleNumber(parameter*real,remainder*real)
    case _=> throw new IllegalArgumentException("No bigM squared allowed")
  }

  override def <(that: MNumber): Boolean = {
    if (parameter < that.parameter) true
    else if (parameter > that.parameter) false
    else remainder < that.remainder
  }

  override def toString: String = MathString{parameter*bigMSymb+remainder}
}