package tests

import core._
import core.operation.{SortExpr, SolveOneEquation}

/**
 * Created by PlussOne on 2015/1/16.
 */
class SolverTests extends MyTest{
  "SolveFor" when{
    val List(a,b,c,x,y,z)= List("a","b","c","x","y","z").map(n=>Symb(n))
    val solver=SolveOneEquation(x)
    "deal with linear equations" should{
      "solve a*x+b" in{
        solver(a*x+b) shouldBe Some(List(-b/a))
      }
      "solve Sin(b)*x*3+a~b" in{
        solver(Sin(b)*x*3-a~b).map(_.map(SortExpr(_))) shouldBe Some(List(SortExpr{a~b/(3*Sin(b))}))
      }
    }
  }
}
