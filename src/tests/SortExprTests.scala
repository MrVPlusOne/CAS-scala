package tests

import core._
import core.operation.SortExpr
import core.patterns.{TimesGroup, PlusGroup}

class SortExprTests extends MyTest{
  "Expr Group" when{
    val symbs=List("a","b","c","d","e","x","y","z").map(Symb)
    val List(a,b,c,d,e,x,y,z)=symbs
    def checkEqual(actual:Expr,expected:Expr)=randomCheckEqual(actual,expected,symbs)
    def throughPlusGroup(in:Expr):Expr= PlusGroup.apply(PlusGroup.unapply(in).get:_*)
    "Is PlusGroup " should{
      "pass 2.1+x+(a+x)+5+Sin(x)+a*5" in{
        val original=NumDouble(2.1)+x+(a+x)+5+Sin(x)+a*5
        checkEqual(throughPlusGroup(original),original)
      }
    }
    "Is TimesGroup" should{
      "match c*(a+b)*sin(x)" in{
        val expr=c*(a+c*b)*Sin(x)
        val expected=List(c,a+c*b,Sin(x))
        TimesGroup.unapply(expr) shouldBe Some(expected)
      }
    }
    def sort(e:Expr)=SortExpr(e)
    "times together" should{
      "a*a=>a^2" in{
        sort{a*a} shouldBe a~2
      }
      "a*a^2=>a^3" in{
        sort{a*a~2} shouldBe a~3
      }
      "b~2*b=>b~3" in{
        sort{b~2*b} shouldBe b~3
      }
      "(a+b)^c*(a+b)=>(a+b)^(c+1)" in{
        sort{(a+b)~c*(a+b)} shouldBe (a+b)~(1+c)
      }
      "(a+b)~(1/3)*Sin(x*x)*(a+b)=>(a+b)~(4/3)*Sin(x~2)" in{
        sort{(a+b)~ One./(3: Expr) *Sin(x*x)*(a+b)} shouldBe Sin(x~2)*(a+b)~Rational(4,3)
      }
      "a*c*b*c~e*a*b~2=>a~2*b~3*c~(1+e)" in{
        sort{a*c*b*c~e*a*b~2} shouldBe a~2*(b~3*c~(1+e))
      }
    }
    "plus together" should{
      "1+2+b=>3+b" in{
        sort{1+(2+b)} shouldBe 3+b
      }
      "a+a=>2*a" in{
        sort{a+a} shouldBe 2*a
      }
      "a+a*2=>3*a" in{
        sort{a+a*2} shouldBe 3*a
      }
      "a*b*(1/3)+b*(2/3)*a=>a*b" in{
        sort{a*b* One./(3: Expr) +b*(NumInt(2)/3)*a} shouldBe a*b
      }
      "a+b+3*a+b=>4*a+2*b" in{
        sort{a+b+3*a+b} shouldBe 4*a+2*b
      }
      "a*b*2+Sin(c+d+c)+a*b=>Sin(2*c+d)+3*a*b" in{
        sort{a*b*2+Sin(c+d+c)+b*a} shouldBe Sin(2*c+d)+3*(a*b)
      }
      "3*a*b~2+a~2*b+a*b~2=>4*a*b~2+a~2*b" in{
        sort{3*a*b~2+a~2*b+a*b~2} shouldBe 4*(a*b~2)+a~2*b
      }
    }
    "division" should{
      "a*b*c~3*(a*c)~2=>a~3*b*c~4" in{
        sort{a*b*c~3*(a*c)~2} shouldBe a~3*(b*c~5)
      }
      "a*b*c~3/(a*c)=>b*c~2" in{
        sort{a*b*c~3/(a*c)} shouldBe b*c~2
      }
    }
  }
}
