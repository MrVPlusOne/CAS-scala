package tests

import core._
import core.operation.{FloatForm, ReplaceSymbs}
import org.scalatest.{ShouldMatchers, WordSpec, FlatSpec}

import scala.util.Random

class MyTest extends WordSpec with ShouldMatchers{
  val Tolerance=1e-6
  def exprAssert(actual:Expr,expected:Expr)(replacements: ReplaceSymbs): Unit ={
    def exprToDouble(expr: Expr):Double= FloatForm(replacements(expr)) match{
      case real:Real=>real.toDouble
      case other=>fail("Can't turn expr into double: %s".format(other))
    }
    val actDouble=exprToDouble(actual)
    val expectDouble=exprToDouble(expected)
    expectDouble should be(actDouble+-Tolerance)
  }

  val random=new Random
  val checkNum=5
  def randomCheckEqual(actual:Expr,expected:Expr,symbols:List[Symb]): Unit ={
    for(i<-0 until checkNum){
      val pairs:List[(Symb,Real)]=symbols.map(_->NumDouble.create(random.nextDouble()))
      val reps=ReplaceSymbs(pairs:_*)
      exprAssert(actual,expected)(reps)
    }
  }
}
