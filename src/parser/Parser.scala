package parser

import core._

import scala.util.matching.Regex
import scala.util.parsing.combinator.{RegexParsers, JavaTokenParsers}
import scala.util.parsing.input.Reader


class ExprParser extends JavaTokenParsers{
  def binaryHandler[T](caseMatch:(Expr,T)=>Expr) = (x:Expr~List[T])=> x match{
    case first~list =>
      list.foldLeft(first)(caseMatch)
  }

  def expr: Parser[Expr] = term ~ rep("+"~term | "-"~term) ^^
    binaryHandler((e1,patt)=>patt match{
      case "+"~e2 => e1+e2
      case "-"~e2 => e1-e2
    })

  def term: Parser[Expr] = factor ~ rep("*"~factor | "/"~factor) ^^
    binaryHandler({
      case (e1,"*"~e2) => e1 * e2
      case (e1, "/"~e2) => e1 / e2
    })

  def factor: Parser[Expr] = (
      value ~ "^" ~ value ^^ { case e1 ~ "^" ~ e2 => e1 ~ e2}
      | value
    )

  def value: Parser[Expr] = parentheses | symb | real

  def symb: Parser[Symb] = """[a-zA-Z]\w*""".r ^^ {case s => s}


  def real: Parser[Real] = (
      """\d+\.\d*""".r ^^ {case f => NumDouble(f.toDouble)}
      | wholeNumber ^^ {case n => NumInt(n.toInt)}
    )
  def parentheses: Parser[Expr] = "("~>expr<~")"
}

object Parser{
  val parser = new ExprParser

  def parseExpr(expr: String) = parser.parseAll(parser.expr, expr)

  def main(args: Array[String]) {
    println{parser.parseAll(parser.expr,"2*a^b+2/3")}
  }
}