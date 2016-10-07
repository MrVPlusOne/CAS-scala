package core.operation

import core._



class DiffInfo(symbol: Symb) {
  def diffSymbol(sym: Symb): Expr = {
    if(sym.name==symbol.name) One
    else Zero
  }
}

case class Diff(info:DiffInfo) extends Transformation{

  override def apply(expr: Expr): Expr = expr match{
    case f2:Func2=>diffF2(f2)
    case f1:Func1=>diffF1(f1)
    case sym:Symb=>info.diffSymbol(sym)
    case _:Real=>Zero
    case other=>throw new UnsupportedOperationException(s"Can't diff this expr: $other")
  }

  def diffF2(func: Func2): Expr = func match{
    case Plus(a,b)=>apply(a) + apply(b)
    case Times(a,b)=>apply(a)*b + a*apply(b)
    case Power(a,b)=>{
      val da=apply(a)
      val db=apply(b)
      val left=if(da==Zero) Zero else b*a~(b-1)*da
      val right=if(db==Zero) Zero else Log(a)*db*Power(a,b)
      left+right
    }
  }

  def diffF1(func:Func1):Expr = apply(func.arg)*func.diffHead

}

