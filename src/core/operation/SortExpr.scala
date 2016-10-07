package core.operation

import core._
import core.patterns.{TimesGroup, PlusGroup}

/**
 * Sort Expr in standardized order
 */
object SortExpr extends Transformation{
  override def apply(expr: Expr): Expr ={
    expr match{
      case PlusGroup(seq) if seq.length>1 => PlusGroup(seq.map(apply).sortWith(plusTermCompare):_*)
      case TimesGroup(seq) if seq.length>1 => TimesGroup(seq.map(apply).sortWith(factorCompare):_*)
      case f:FuncOnExpr=>f.foreachArg(apply)
      case other=>other //No need to change
    }
  }
  
  def plusTermCompare(a:Expr,b:Expr):Boolean= {
    val seqA=a match{
      case TimesGroup(seq)=>seq.dropWhile(_.isInstanceOf[Real])
    }
    val seqB=b match{
      case TimesGroup(seq)=>seq.dropWhile(_.isInstanceOf[Real])
    }
    if(seqA.length==seqB.length){
      def loop(seq:Seq[(Expr,Expr)]):Boolean=
        if(seq.isEmpty) false
        else {
          val (s1,s2)=seq.head
          if(s1==s2) loop(seq.tail)
          else factorCompare(s1,s2)
        }
      loop(seqA zip seqB)
    }else{
      seqA.length<seqB.length
    }
  }
  def factorCompare(a:Expr,b:Expr):Boolean={
    def toPower(e:Expr):Power=e match{
      case p:Power=>p
      case other=>Power(other,One)
    }
    def dropReal(expr: Expr)=expr match{
      case Times(r:Real,e)=>e
      case _=>expr
    }
    val (pa,pb)= (toPower(a),toPower(b))
    val (facA,facB)=(dropReal(pa.e1),dropReal(pb.e1))
    if(facA==facB){
      pa.e2 isLeftTo pb.e2
    }else{
      facA isLeftTo facB
    }
  }
}
