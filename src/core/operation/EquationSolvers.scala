package core.operation

import core.{Expr, Symb}
import core.patterns.Polynomial

case class SolveOneEquation(unknown:Symb){
  def apply(equation: Expr): Option[List[Expr]] = {
    val polynomial=Polynomial(unknown)
    equation match{
      case polynomial(params)=>{
        val order=params.length-1
        order match{
          case 0=>Some(Nil)
          case 1=>Some(List(-params(0)/params(1)))
          case 2=> ???
          case 3=> ???
          case _=> None
        }
      }
      case _=>None //can't solve other equations yet.
    }
  }
}

object SolveOneEquation{

}