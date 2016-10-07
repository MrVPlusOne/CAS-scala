package core.operation

import core._

case class ReplaceSymbs(reps:(Symb,Expr)*) extends Transformation{
  val map:Map[Symb,Expr]=Map(reps:_*)
  override def apply(original: Expr): Expr = original match{
    case v:Symb=> map.getOrElse(v,v)
    case func:FuncOnExpr=>func.foreachArg(apply)
    case c:Real=>c
  }
}
