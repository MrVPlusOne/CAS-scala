package core.patterns

import core._

/**
 * make sure the expr being matched is sorted.
 * @param symb
 */
case class SymbTerm(symb:Symb){
  def apply(parameter:Expr,power:Expr):Expr = parameter*symb~power
  def unapply(expr:Expr):Option[(Expr,Expr)] = expr match{
    case TimesGroup(seq)=>{
      def loop(left:Seq[Expr],n:Int):Option[(Expr,Expr)]={
        def makeParameter=TimesGroup(seq.take(n)++left.tail:_*)

        if(left.isEmpty) None
        else left.head match{
          case `symb`=>Some(makeParameter,One)
          case Power(`symb`,p)=>Some(makeParameter,p)
          case _=>loop(left.tail,n+1)
        }
      }

      loop(seq,0)
    }
  }
}


