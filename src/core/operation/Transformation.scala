package core.operation

import core.Expr


abstract class Transformation{
  def apply(expr:Expr):Expr
}
