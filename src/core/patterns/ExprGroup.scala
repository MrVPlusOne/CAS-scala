package core.patterns

import core._
import core.operation.SortExpr

/**
 * any Expr can be treat as any ExprGroup
 */
trait ExprGroup{
  def seed:Expr
  def connect(result:Expr,newExpr:Expr):Expr
  def turnToFunc2(expr:Expr):Option[Func2]

  def apply(exprs:Expr*):Expr= exprs.foldRight(seed)((next,result)=>connect(result,next))

  def unapply(expr:Expr):Some[Seq[Expr]]= {
    def traversal(result: List[Expr], l: Expr, r: Expr): List[Expr] = {
      val rightResult=turnToFunc2(r) match {
        case Some(f2) => traversal(result,f2.e1,f2.e2)
        case None=>r::result
      }
      turnToFunc2(l) match {
        case Some(f2) => traversal(rightResult, f2.e1, f2.e2)
        case None => l :: rightResult
      }
    }

    turnToFunc2(expr) match{
      case Some(f2)=>Some(traversal(Nil,f2.e1,f2.e2))
      case None=>Some(List(expr))
    }
  }
}

/**
 * any expr can be treated as a PlusGroup
 */
object PlusGroup extends ExprGroup{
  override def seed: Expr = Zero

  override def turnToFunc2(expr: Expr): Option[Func2] = expr match{
    case plus:Plus=>Some(plus)
    case _=>None
  }

  override def connect(result: Expr, newExpr: Expr): Expr = {
    def combine(e1:Expr,e2:Expr):(Expr,Option[Expr])= {
      if(e1.isInstanceOf[Real]&&e2.isInstanceOf[Real]) (e1+e2,None)
      else {
        val Some((n1, t1)) = ExprTerm.unapply(e1)
        val Some((n2, t2)) = ExprTerm.unapply(e2)
        if (t1 == t2) (SortExpr(n1 + n2) * t1, None)
        else (e1, Some(e2))
      }
    }

    result match{
      case Plus(head,tail)=>{
        val (e1,ope2)=combine(head,newExpr)
        ope2 match{
          case Some(e2)=>e2+(e1+tail)
          case None=>e1+tail
        }
      }
      case _=>combine(result,newExpr) match{
        case (e1,Some(e2))=>e2+e1
        case (e1,None)=>e1
      }
    }
  }


}

/**
 * any expr can be treated as a TimesGroup
 */
object TimesGroup extends ExprGroup{
  override def seed: Expr = One

  override def turnToFunc2(expr: Expr): Option[Func2] =expr match{
    case times:Times=>Some(times)
    case _=>None
  }

  override def connect(result: Expr, newExpr: Expr): Expr = {
    def combine(e1:Expr,e2:Expr):(Expr,Option[Expr])= {
      if(e1.isInstanceOf[Real]&&e2.isInstanceOf[Real]) (e1*e2,None)
      else {
        val Some((b1, p1)) = ExprFactor.unapply(e1)
        val Some((b2, p2)) = ExprFactor.unapply(e2)
        if (b1 == b2) (b1 ~ SortExpr(p1 + p2), None)
        else (e1, Some(e2))
      }
    }

    result match{
      case Times(head,tail)=>{
        val (e1,ope2)=combine(head,newExpr)
        ope2 match{
          case Some(e2)=>e2*(e1*tail)
          case None=>e1*tail
        }
      }
      case _=>combine(result,newExpr) match{
        case (e1,Some(e2))=>e2*e1
        case (e1,None)=>e1
      }
    }
  }
}

object ExprTerm{
  def apply(num:Expr,term:Expr):Expr=num*term

  def unapply(expr: Expr):Option[(Expr,Expr)]=expr match{
    case Times(n:Real,term:Expr)=>Some(n,term)
    case _=>Some(One,expr)
  }
}

object ExprFactor{
  def apply(b:Expr,p:Expr):Expr= b~p

  def unapply(expr: Expr):Option[(Expr,Expr)]=expr match{
    case Power(b,p)=>Some(b,p)
    case other=>Some(other,One)
  }
}
