package core.operation

import core.{Times, Expr}
import core.patterns.PlusGroup

object Simplify extends Transformation{
  def compareExpr(e1:Expr,e2:Expr):Boolean= ???


  override def apply(expr: Expr): Expr = expr match{
    case PlusGroup(seq)=> {
      seq.sortWith(???)
      ???
    }
    case _=>expr //unchanged
  }
}
