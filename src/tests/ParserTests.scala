package tests

import core.{Rational, NumDouble, Expr, Symb}
import parser.ExprParser

/**
 * Created by weijiayi on 6/27/15.
 */
class ParserTests extends MyTest{

  "parsing tests" should {
    val List(a,b,c,d,e,f) = List("a","b","c","d","e","f").map(Symb)

    "test terms" in {
      val parser = new ExprParser
      def parse(exprString: String) = parser.parseAll(parser.expr, exprString).get

      val testTable = List(
        "a+b-c+d" -> (a+b-c+d) ,
        ("a*b+c",a*b+c),
        ("c+a*b",c+a*b),
        ("a-b*c-e^f",a-b*c-e~f),
        ("a^(b-c)/d-e",a~(b-c)/d-e),
        ("1.5*a^b+2/3",NumDouble(1.5)*a~b+Rational(2,3))
      )
      for((s,e) <- testTable){
        parse(s) shouldBe e
      }
    }
  }
}
