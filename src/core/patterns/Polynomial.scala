package core.patterns

import core._

import scala.collection.mutable

/**
 * match a polynomial of variable symb (which means all the powers of symb shouldBe integer)
 * require Expr being sorted
 * @param symb
 */
case class Polynomial(symb:Symb) {
  def pow(n:Int)=symb~n
  def apply(parameters:Expr*):Expr={
    def loop(result:Expr,left:Seq[Expr],p:Int):Expr={
      if(left.isEmpty) result
      else loop(result+left.head*pow(p),left.tail,p+1)
    }
    loop(Zero,parameters,0)
  }

  val symbTerm=SymbTerm(symb)
  val freeOfSymb=new FreeOfSymb(symb)
  /**
   *
   * @param expr sorted expr
   * @return a sequence of the parameters, thus Seq(a0,a1,a2,...)
   */
  def unapply(expr: Expr):Option[List[Expr]]= {

    expr match{
      case PlusGroup(seq)=> {
        val map = new mutable.HashMap[Int, Expr]
        map += (0 -> Zero)
        for (factor <- seq) {
          factor match {
            case symbTerm(para, power) if NumInt.isInt(power) && freeOfSymb.unapply(para) => {
              val p = power match {
                case NumInt(int) => int
              }
              if(p<0) return None //Can't accept minus power factor
              map.get(p) match {
                case Some(e) => map(p) += para
                case None => map += (p -> para)
              }
            }
            case e if freeOfSymb.unapply(e) => map(0) += e
            case _ => return None
          }
        }
        val highestOrder=map.keys.max
        Some((0 to highestOrder).map(i=>map.get(i) match{
          case Some(e)=>e
          case None=>Zero
        }).toList)
      }
    }
  }
}
