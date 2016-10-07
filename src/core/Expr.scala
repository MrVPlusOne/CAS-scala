package core

import core.Real.Generality

abstract sealed class Expr {
  def isLeftTo(expr:Expr):Boolean

  def +(expr:Expr):Expr= expr match{
    case Zero=>this
    case _=>Plus(this,expr)
  }
  def *(expr: Expr):Expr=expr match{
    case Zero=>Zero
    case One=>this
    case _=>Times(this,expr)
  }
  def /(expr:Expr):Expr=expr match{
    case One=>this
    case Zero=>throw new RuntimeException("Divided by zero!")
    case _=>this*expr~(-1)
  }
  def -(expr:Expr):Expr= this + expr*(-1)
  def unary_- :Expr= this* -1

  def ~(power:Expr):Expr=power match{
    case Zero=>One
    case One=>this
    case _ => Power(this,power)
  }

  def exprString:String

  override def toString=exprString
}

object Expr{
  implicit def int2Real (int:Int):Real = NumInt(int)

  implicit def double2Real(double: Double):Real=NumDouble(double)

  implicit def string2Symbol (str:String):Symb = Symb(str)
}

object CallerPrecedence extends Enumeration{
  val Plus,Times,Power,FuncCall=Value
}

case class Symb(name:String) extends Expr {

  override def exprString: String = name

  override def isLeftTo(expr: Expr): Boolean = expr match{
    case r:Real=>false
    case s:Symb=>name<s.name
    case f:FuncOnExpr=>true
  }

}

abstract class Real extends Expr{

  override final def isLeftTo(expr: Expr): Boolean = expr match{
    case r:Real=>toDouble<r.toDouble
    case _=>true
  }

  def toDouble:Double
  implicit def generality:Generality
  def moreGeneral(implicit that: Generality):Boolean=generality.g>that.g

  def equalTo(real:Real,tolerance:Double=1e-8) = math.abs(toDouble-real.toDouble)<tolerance

  def < (that:Real) = toDouble<that.toDouble
  def > (that:Real) = toDouble>that.toDouble
  def <= (that:Real) = ! this.>(that)

  def +(expr:Real):Real = (this+expr.asInstanceOf[Expr]).asInstanceOf[Real]
  def *(expr:Real):Real = (this*expr.asInstanceOf[Expr]).asInstanceOf[Real]
  def /(expr:Real):Real = (this/expr.asInstanceOf[Expr]).asInstanceOf[Real]
  def -(expr:Real):Real = (this-expr.asInstanceOf[Expr]).asInstanceOf[Real]

  def ~(power:Real):Real = (this~power.asInstanceOf[Expr]).asInstanceOf[Real]
}

abstract class FuncOnExpr extends Expr{
  def holdForAllArg(exprToBoolean: (Expr) => Boolean): Boolean

  def foreachArg(f:Expr=>Expr):Expr
  
  def tryFloatForm:Expr
}

object Real{

  val Infinity = NumDouble(Double.PositiveInfinity)
  class Generality(val g:Int)

  def generality_int=new Generality(0)
  def generality_rational=new Generality(1)
  def generality_double=new Generality(2)
  def generality_top=new Generality(10)

  val ordering = new Ordering[Real]{
    override def compare(x: Real, y: Real): Int = {
      if( x<y ) -1
      else if(y<x) 1
      else 0
    }
  }

}






