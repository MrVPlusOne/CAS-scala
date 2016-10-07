package core

import core.patterns.{ExprFactor, TimesGroup}

trait Func2 extends FuncOnExpr{
  def func2Precedence:Int

  override def isLeftTo(expr: Expr): Boolean = expr match{
    case f2:Func2=>if(func2Precedence==f2.func2Precedence) {
      e1.isLeftTo(f2.e1)|| (!f2.e1.isLeftTo(e1) && e2.isLeftTo(f2.e2))
    }else{
      func2Precedence<f2.func2Precedence
    }
    case _=>false
  }

  val e1:Expr
  val e2:Expr
  def createFromArgs(arg1:Expr,arg2:Expr):Expr
  def doubleFunc(d1:Double,d2:Double):Double

  override def foreachArg(f: Expr => Expr): Expr = createFromArgs(f(e1),f(e2))
  
  override def holdForAllArg(f: (Expr) => Boolean): Boolean = f(e1)&&f(e2)

  override def tryFloatForm: Expr = (e1,e2) match {
    case (r1:Real,r2:Real)=>NumDouble(doubleFunc(r1.toDouble,r2.toDouble))
    case _=>this
  }
}

case class Plus(e1:Expr,e2:Expr) extends Func2 {

  override def exprString: String = "(%s+%s)".format(e1.exprString,e2.exprString)

  override def createFromArgs(arg1: Expr, arg2: Expr): Expr = arg1+arg2

  override def doubleFunc(d1: Double, d2: Double): Double = d1+d2

  override def func2Precedence: Int = 1

}

case class Times(e1:Expr,e2:Expr) extends Func2 {

  override def ~(power: Expr): Expr = power match{
    case NumInt(n) if n>1=> this match{case TimesGroup(seq)=>TimesGroup(seq.map(_~power):_*)}
    case NumInt(n) if n<0=> this match{case TimesGroup(seq)=>TimesGroup(seq.map(a=>One/a~(-n)):_*)}
    case _=>super.~(power)
  }

  def dropReal:Expr = if(e1.isInstanceOf[Real]) e2 else this

  override def isLeftTo(expr: Expr): Boolean = expr match{
    case symb:Symb=> !symb.isLeftTo(this)
    case times:Times=>dropReal isLeftTo times.dropReal
    case _=>super.isLeftTo(expr)
  }

  override def exprString: String = "(%s*%s)".format(e1.exprString,e2.exprString)

  override def createFromArgs(arg1: Expr, arg2: Expr): Expr = arg1*arg2

  override def doubleFunc(d1: Double, d2: Double): Double = d1*d2

  override def func2Precedence: Int = 2
}

case class Power(e1:Expr,e2:Expr) extends Func2{
  override def createFromArgs(arg1: Expr, arg2: Expr): Expr = arg1~arg2

  override def doubleFunc(d1: Double, d2: Double): Double = math.pow(d1,d2)

  override def exprString: String = "%s^%s".format(e1.exprString,e2.exprString)

  override def func2Precedence: Int = 3

  override def isLeftTo(expr: Expr): Boolean = expr match{
    case symb:Symb=> !symb.isLeftTo(this)
    case ExprFactor(b,p)=> e1 isLeftTo b
    case _=>super.isLeftTo(expr)
  }
}

object Power{
  def intPow(b:Int,p:Int):Int= pow[Int](b,p,_*_)
  def pow[T](b:T,p:Int,combF:(T,T)=>T):T={
    require(p>0,"p should be greater than 0 in intPow")
    if(p==1) b
    else{
      val newB=combF(b,b)
      val newP=p/2
      if(p%2==0){
        pow(newB,newP,combF)
      }else{
        combF(b,pow(newB,newP,combF))
      }
    }
  }
}

