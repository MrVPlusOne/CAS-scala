package core

trait Func1 extends FuncOnExpr {

  override def isLeftTo(expr: Expr): Boolean = expr match{
    case f2:Func2=>true
    case f1:Func1=>if(funcName==f1.funcName){
      arg.isLeftTo(f1.arg)
    }else{
      funcName<f1.funcName
    }
    case _=>false
  }

  val arg:Expr
  def createFromArg(arg: Expr): Expr

  override def foreachArg(f: (Expr) => Expr): Expr = createFromArg(f(arg))

  override def holdForAllArg(exprToBoolean: (Expr) => Boolean): Boolean = exprToBoolean(arg)

  def diffHead: Expr

  def funcName: String

  def numericFunc(d:Double):Double

  override def exprString: String = "%s(%s)".format(funcName,arg.exprString)

  override def tryFloatForm: Expr = arg match{
    case r:Real=>NumDouble(numericFunc(r.toDouble))
    case _=>this
  }
}

case class Sin(arg:Expr) extends Func1{
  override def diffHead: Expr = Cos(arg)

  override def createFromArg(arg: Expr): Expr = Sin(arg)

  override def funcName: String = "sin"

  override def numericFunc(d: Double): Double = math.sin(d)
}

case class Cos(arg:Expr) extends Func1 {
  override def diffHead: Expr = -1 * Sin(arg)

  override def funcName: String = "cos"

  override def createFromArg(arg: Expr): Expr = Cos(arg)

  override def numericFunc(d: Double): Double = math.cos(d)
}

case class Exp(arg:Expr) extends Func1{
  override def createFromArg(arg: Expr): Expr = Exp(arg)

  override def funcName: String = "exp"

  override def numericFunc(d: Double): Double = math.exp(d)

  override def diffHead: Expr = this
}

case class Log(arg:Expr) extends Func1{
  override def createFromArg(arg: Expr): Expr = Log(arg)

  override def funcName: String = "log"

  override def numericFunc(d: Double): Double = math.log(d)

  override def diffHead: Expr = One/arg
}

case class Tan(arg:Expr) extends Func1{
  override def createFromArg(arg: Expr): Expr = Tan(arg)

  override def funcName: String = "tan"

  override def numericFunc(d: Double): Double = math.tan(d)

  override def diffHead: Expr = Cos(arg)~(-2)
}