import core._
import core.operation._
import core.patterns.Polynomial


object PlayAround {
  val diff=Diff(new DiffInfo("x"))
  val symbs=List("a","b","c","d","e","x","y","z").map(Symb)
  val List(a,b,c,d,e,x,y,z)=symbs


  def simplifyString(expr: Expr): String ={
    MathString(SortExpr(ExpandExpr(expr)))
  }

  def printMathString(expr:Expr): Unit ={
    println(s">>> ${MathString(SortExpr(expr))}")
  }
  def solveSingleEquation(equation: Expr,unknown:Symb=x): Unit ={
    println{s"[Solve equation] ${MathString(equation)} = 0"}
    println{s"$unknown = ${simplifyString(SolveOneEquation(unknown)(equation).get.head)}"}
  }
  def diffExpr(expr: Expr,variable:Symb=x): Expr ={
    println{s"[d/d$variable] ${MathString(expr)}"}
    val sortExpr: Expr = SortExpr(diff(expr))
    println{s">>> ${MathString(sortExpr)}"}
    sortExpr
  }
  def outPut(expr: Expr): Unit ={
    println(">>> "+simplifyString(expr))
  }

  def main(args: Array[String]) {
    welcome()

    val result=diffExpr(Tan(a*x~(b+1)))
    println(s"[b => 5] ${MathString(result)}")
    outPut{ReplaceSymbs(b->NumInt(5)){result}}
    println(s"[Expand Polynomial] ${MathString((a+b)~3)}")
    outPut{ExpandExpr((a+b)~3)}
    solveSingleEquation (Sin(a)+b*x+c)
    solveSingleEquation (-3*a+a~3+6*a*x)

    println{MathString{a/(b*c)+c }}
    solveSingleEquation (-3*a+a~3+6*a*x)
  }

  def welcome(): Unit ={
    println("Welcome to Severus Raindrop, a computer algebra system written by Jiayi Wei.\nCurrent version --0.2.0")
    println("----------------------------")
  }
}
