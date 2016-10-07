package core.operation

import core.{Real, FuncOnExpr, Expr}

object FloatForm extends Transformation{
  override def apply(expr: Expr): Expr = expr match {
    case func:FuncOnExpr=>{
      func.foreachArg(apply) match{
        case real:Real=>real
        case stillFunc:FuncOnExpr=>stillFunc.tryFloatForm
        case other=>other
      }

    }
    case _=>expr
  }
}
