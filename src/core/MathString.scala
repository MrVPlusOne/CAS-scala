package core

import core.patterns.{ExprFactor, PlusGroup, TimesGroup}

/**
 * Return the nice string form of an Expr
 */
object MathString {
  def apply(expr: Expr):String={

    val buffer=new StringBuilder

    val call_func=0
    val call_plus=1
    val call_times=2
    val call_power=3

    def append(str:String):Unit=buffer.append(str)

    def buildSeqOp(operator: Char,caller:Int)(seq: Seq[Expr]): Unit ={
      build(seq.head, caller)
      seq.tail.foreach(e => {
        buffer.append(operator)
        build(e, caller)
      })
    }

    def wrapInParentheses(need:Boolean)(action: ()=>Unit): Unit ={
      if(need){
        buffer.append('(')
        action()
        buffer.append(')')
      }else{
        action()
      }
    }

    def build(expr: Expr,caller:Int): Unit ={
      expr match{
        case Symb(name)=>append(name)
        case r:Real=>append(r.exprString)
        case Power(b,p)=>{
          build(b,call_power)
          buffer.append('^')
          build(p,call_power)
        }
        case TimesGroup(seq) if seq.length>1=>{
          def hasMinusPower(expr: Expr)=expr match{
            case ExprFactor(b,NumInt(p))=> p<0
            case _=>false
          }
          def turnToPosPower(expr: Power)=expr match{
            case Power(b,p) => b~(-1*p)
          }
          def buildSeq=buildSeqOp('*',call_times)(_)

          //Start
          wrapInParentheses(caller>call_times)(()=> {
            val dropped = seq.head match {
              case NumInt(-1) => append("-"); seq.tail
              case _ => seq
            }
            val minusPower = dropped.filter(hasMinusPower).map(p=>turnToPosPower(p.asInstanceOf[Power]))
            if (minusPower.isEmpty) {
              buildSeq(dropped)
            } else {
              val posPower = dropped.filter(!hasMinusPower(_))
              if (posPower.isEmpty) append("1")
              else {
                buildSeq(posPower)
                wrapInParentheses (minusPower.length > 1)(()=> {
                  append("/")
                  buildSeq(minusPower)
                })
              }
            }
          })
        }
        case PlusGroup(seq) if seq.length>1=>{
          wrapInParentheses(caller>call_plus)(()=> {
            buildSeqOp('+',call_plus)(seq)
          })
        }
        case f1:Func1=>{
          append(f1.funcName+"(")
          build(f1.arg,call_func)
          append(")")
        }
      }
    }

    build(expr,call_func)
buffer.mkString.replace("+-","-")
  }
}
