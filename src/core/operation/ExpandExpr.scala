package core.operation

import core._
import core.patterns.PlusGroup

/**
 * Expand an Expr into sum form
 */
object ExpandExpr extends Transformation{

  def timesAndExpand(as:Expr,bs:Expr):Expr={
    def toPlusList(expr:Expr):Seq[Expr]=expr match{
      case PlusGroup(seq)=>seq
    }

    PlusGroup(toPlusList(as).flatMap(a=>toPlusList(bs).map(a*_)):_*)
  }
  
  override def apply(expr: Expr): Expr = {
    val expandInside=expr match{
      case f:FuncOnExpr=>f.foreachArg(apply)
      case _=>expr
    }
    expandInside match{
      case Times(as,bs)=> timesAndExpand(as,bs)
      case Power(b,p:NumInt) if p.value>1 => Power.pow[Expr](b,p.value,timesAndExpand)
      case other=>other
    } 
  }
}
