package tests

import core._
import core.operation.SortExpr
import core.patterns.{FreeOfSymb, Polynomial, SymbTerm}


class ExprPatternTests extends MyTest{
  "Expr Pattern" when {
    val List(a, b, c, x, y, z) = List("a,", "b", "c", "x", "y", "z").map(Symb)

    "A SymbTerm pattern of x" should {
      val pattern = new SymbTerm(x)
      "match x" in {
        x match {
          case pattern(para, pow) => para shouldBe One
        }
      }
      "match a*x" in {
        a * x match {
          case pattern(para, pow) => {
            para shouldBe a
            pow shouldBe One
          }
        }
      }
      "match x*Sin(x)*a*x~2" in {
        SortExpr(x * Sin(x) * a * x ~ 2) match {
          case pattern(para, pow) => {
            para shouldBe a * Sin(x)
            pow shouldBe NumInt(3)
          }
        }
      }
      "not match x+a" in {
        x + a match {
          case pattern(para, pow) => fail()
          case _ => "pass"
        }
      }
    }

    "FreeOfSymb pattern of x" should{
      val pattern = new FreeOfSymb(x)
      "match x+Sin(a)*b-x" in{
        pattern.unapply(SortExpr(x+Sin(a)*b-x)) shouldBe true
      }
      "not match Sin(a+x)*b+c" in{
        pattern.unapply(Sin(a+x)*b+c) shouldBe false
      }
    }
    "A polynomial pattern of x" should{
      val pattern=Polynomial(x)
      "match a+b*x+c*x~2 to List(a,b,c)" in{
        a+b*x+c*x~2 match{
          case pattern(list)=>list shouldBe List(a,b,c)
        }
      }
      "not match a+b*Sin(x)*x" in{
        a+b*Sin(x)*x match{
          case pattern(_)=>fail("this shouldn't match polynomial")
          case _=> "pass"
        }
      }
    }
  }
}
