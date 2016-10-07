package tests

import core.{NumInt => int, Rational, Expr}

import scala.util.Random


class NumericCalcTest extends MyTest{
  "NumInt" when{
    val random=new Random
    val testNum=5
    val bound=1000
    def randInt():Int=random.nextInt(bound)

    "calc with NumInt" should{
      def twoFuncEqual(f1:(Int,Int)=>Expr,f2:(Int,Int)=>Expr):Unit={
        for(i<-0 until testNum){
          val a=randInt()
          val b=randInt()
          f1(a,b) shouldBe f2(a,b)
        }
      }
      "return sum" in{
        twoFuncEqual((a,b)=>int(a)+int(b),(a,b)=>int(a+b))
        twoFuncEqual((a,b)=>int(a)-int(b),(a,b)=>int(a-b))
        twoFuncEqual((a,b)=>int(a)*int(b),(a,b)=>int(a*b))
      }
      "return rational" in{
        twoFuncEqual((a,b)=>int(a)/int(b),(a,b)=>Rational(a,b))
      }
    }

    "calc with Rational" should{
      def twoFuncEqual(f1:(Int,Rational)=>Expr,f2:(Int,Rational)=>Expr): Unit ={
        for(i<-0 until testNum){
          val a=randInt()
          val r=Rational.createRational(randInt(),randInt())
          f1(a,r) shouldBe f2(a,r)
        }
      }
      "return sum" in{
        twoFuncEqual((a,r)=>int(a)+r,(a,r)=>Rational(a*r.denom+r.num,r.denom))
        twoFuncEqual((a,r)=>int(a)-r,(a,r)=>Rational(a*r.denom-r.num,r.denom))
        twoFuncEqual((a,r)=>int(a)*r,(a,r)=>Rational(a*r.num,r.denom))
        twoFuncEqual((a,r)=>int(a)/r,(a,r)=>Rational(a*r.denom,r.num))
      }
      "pass as power" in{
        Rational(7,3)~3 shouldBe Rational(7*7*7,3*3*3)
      }
    }
  }

}
