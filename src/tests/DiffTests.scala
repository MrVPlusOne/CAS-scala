package tests

import core.operation.{DiffInfo, Diff}
import core._

class DiffTests extends MyTest {
  "Diff r.p.t. x "should{
    val symbs=List("a","b","c","d","e","x","y","z").map(Symb)
    val List(a,b,c,d,e,x,y,z)=symbs

    val diff=Diff(new DiffInfo("x"))

    def diffCheck(original:Expr,derivative:Expr): Unit ={
      randomCheckEqual(diff(original),derivative,symbs)
    }

    "pass sin(a*x*b)" in{
      diffCheck(Sin(a*x*b),a*b*Cos(a*x*b))
    }

    "pass sin(a x) cos(b x)" in{
      diffCheck(Sin(a*x)*Cos(b*x),a*Cos(a*x)*Cos(b*x)+Sin(a*x)*(-1)*b*Sin(b*x))
    }

    "pass exp(sin(a+x))" in{
      diffCheck(Exp(Sin(a+x)),Exp(Sin(a+x))*Cos(a+x))
    }

    "pass sin(x)/cos(a x)" in{
      diffCheck(Sin(x)/Cos(a*x),Cos(x)/Cos(a*x)+a*Sin(x)*Sin(x*a)/(Cos(a*x)*Cos(a*x)))
    }

    "pass log(a x)" in{
      diffCheck(Log(a*x),a/(a*x))
    }

    "pass tan(sin(x)+log(b))" in{
      diffCheck(Tan(Sin(x)+Log(b)),diff(Sin(Sin(x)+Log(b))/Cos(Sin(x)+Log(b))))
    }
  }
}
