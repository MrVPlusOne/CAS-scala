package core.patterns

import core.{FuncOnExpr, Expr, Symb}

/**
 * Created by PlussOne on 2015/1/17.
 */
case class FreeOfSymb(symb:Symb) {
  def unapply(expr: Expr):Boolean= expr match{
    case Symb(name)=>name!=symb.name
    case f:FuncOnExpr=>f.holdForAllArg(unapply)
    case _=>true
  }
}
