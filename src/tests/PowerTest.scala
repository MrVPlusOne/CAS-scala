package tests

import core._

class PowerTest extends MyTest{
  "intPow test" should{
    import Power.intPow
    "pass 5^3" in{
      intPow(5,3) shouldBe 5*5*5
    }
    "pass 7^7" in{
      intPow(7,7) shouldBe 7*7*7*7*7*7*7
    }
    "throw exception" in{
      intercept[IllegalArgumentException]{
        intPow(5,-3)
      }
    }
  }
  "two equal expr" should{
    val symbs=List("a","b","c","d","e","x","y","z").map(Symb)
    val List(a,b,c,d,e,x,y,z)=symbs
    def checkEqual(actual:Expr,expected:Expr)=randomCheckEqual(actual,expected,symbs)

    "pass x^3" in{
      checkEqual(x~3,x*x*x)
    }

    "pass (a x)^-3" in{
      checkEqual((a*x)~(-3),1/(a*a*a*x*x*x))
    }
  }
}
