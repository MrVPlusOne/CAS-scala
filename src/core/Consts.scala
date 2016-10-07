package core

import java.text.{DecimalFormat, NumberFormat}

import Real.Generality


class NumInt private (val value:Int) extends Real {

  override def +(expr: Expr): Expr = expr match{
    case NumInt(that)=> value+that
    case real:Real=>real + this
    case _=>super.+(expr)
  }

  override def *(expr: Expr): Expr = expr match{
    case NumInt(int)=> value*int
    case real:Real=>real * this
    case _=>super.*(expr)
  }

  override def /(expr: Expr): Expr = expr match{
    case NumInt(int)=>Rational(value,int)
    case _=>super./(expr)
  }

  override def ~(power: Expr): Expr = {
    power match{
      case NumInt(int) =>
        if (int>0) Power.intPow(value,int)
        else if(int<0) One./(Power.intPow(value,-int):Expr)
        else Zero
      case _=>super.~(power)
    }
  }

  override def exprString: String = value.toString

  override def toDouble: Double = value

  override implicit def generality: Generality = Real.generality_int

  override def equals(obj: scala.Any): Boolean = obj match{
    case NumInt(int)=>value==int
    case _=>false
  }

  override def hashCode(): Int = value
}

object NumInt{
  def apply(int:Int):Real = int match{
    case 0=>Zero
    case 1=>One
    case _=>new NumInt(int)
  }

  def unapply(expr: Expr):Option[Int]=expr match{
    case num:NumInt=>Some(num.value)
    case Zero=>Some(0)
    case One=>Some(1)
    case _=>None
  }

  def create(int:Int)=new NumInt(int)

  def isInt(expr: Expr):Boolean=expr match{
    case NumInt(_)=>true
    case _=>false
  }
}


class Rational private (val num:Int,val denom:Int) extends Real{
  override def toDouble: Double = num.toDouble/denom

  override def exprString: String = "%d/%d".format(num,denom)

  override implicit def generality: Generality = Real.generality_rational

  override def toString: String = "Rational(%d,%d)".format(num,denom)

  override def equals(obj: scala.Any): Boolean = obj match{
    case r:Rational=>denom==r.denom&&num==r.num
    case _=>false
  }

  override def hashCode(): Int = num+denom

  override def +(expr: Expr): Expr = expr match{
    case NumInt(int)=>Rational(num+int*denom,denom)
    case r:Rational=>Rational(num*r.denom+r.num*denom,denom*r.denom)
    case real:Real=>real + this
    case _=>super.+(expr)
  }

  override def *(expr: Expr): Expr = expr match{
    case NumInt(int)=>Rational(num*int,denom)
    case r:Rational=>Rational(num*r.num,denom*r.denom)
    case real:Real=>real*this
    case _=>super.*(expr)
  }

  override def /(expr: Expr): Expr = expr match{
    case NumInt(int)=>Rational(num,denom*int)
    case r:Rational=>Rational(num*r.denom,denom*r.num)
    case _=>super./(expr)
  }

  import Power.intPow
  override def ~(power: Expr): Expr = power match{
    case NumInt(int)=>
      if(int==0) {
        if (num == 0) throw new RuntimeException("0^0 encountered.")
        else One
      }else if(int>0) {
        Rational(intPow(num,int),intPow(denom,int))
      }else{
        val abs= -int
        Rational(intPow(denom,abs),intPow(num,abs))
      }
    case _=>super.~(power)
  }
}

object Rational{
  private def gcd(a:Int,b:Int):Int=if(b==0) a else gcd(b,a%b)

  def apply(n:Int,d:Int):Real={
    if(n==d){
      require(n!=0,"0/0 encountered!")
      One
    }else if(n==0){
      Zero
    }else{
      val g=gcd(n,d)
      val gg:Int=if(g<0) -g else g
      val denum=d/gg
      val num=n/gg
      if(denum==1) NumInt(num)
      else new Rational(num, denum)
    }
  }

  def unapply(expr: Expr):Option[(Int,Int)]=expr match{
    case r:Rational=>Some(r.num,r.denom)
    case Zero=>Some(0,1)
    case One=>Some(1,1)
    case _=>None
  }

  def createRational(n:Int,d:Int):Rational={
    val g=gcd(n,d)
    new Rational(n/g,d/g)
    new Rational(n,d)
  }
}

class NumDouble private (val value:Double) extends Real{
  override def equals(obj: scala.Any): Boolean = obj match{
    case NumDouble(d)=>value==d
    case _=>false
  }

  override def hashCode(): Int = value.hashCode()

  override def +(expr: Expr): Expr = expr match{
    case r:Real=>NumDouble(value+r.toDouble)
    case _=>super.+(expr)
  }

  override def *(expr: Expr): Expr = expr match{
    case r:Real=>NumDouble(value*r.toDouble)
    case _=>super.*(expr)
  }

  override def ~(p: Expr): Expr = p match{
    case r:Real=>NumDouble(math.pow(value,r.toDouble))
    case _=>super.~(p)
  }

  override def toDouble: Double = value

  override def exprString: String = NumDouble.formatter.format(value)

  override implicit def generality: Generality = Real.generality_double
}

object NumDouble{
  def create(int: Double):NumDouble = new NumDouble(int)

  val formatter=new DecimalFormat(".#####")
  def apply(double: Double):Real=double match{
    case 0.0=>Zero
    case 1.0=>One
    case _=>new NumDouble(double)
  }
  def unapply(expr: Expr):Option[Double]=expr match{
    case d:NumDouble=>Some(d.value)
    case r:Real=>Some(r.toDouble)
    case _=>None
  }
}

object Zero extends Real{

  override def +(expr: Expr): Expr = expr

  override def *(expr: Expr): Expr = Zero

  override def /(expr: Expr): Expr = if(expr!=Zero) Zero else throw new RuntimeException("0/0 encountered!")

  override def ~(power: Expr): Expr = if(power!=Zero) Zero else throw new RuntimeException("0^0 encountered!")

  override def exprString: String = "0"

  override def toDouble: Double = 0.0

  override implicit def generality: Generality = Real.generality_top
}

object One extends Real{
  val asInt=NumInt.create(1)
  override def +(expr: Expr): Expr = expr match{
    case NumInt(int)=>NumInt(int+1)
    case NumDouble(double)=>NumDouble(double+1)
    case _=>super.+(expr)
  }

  override def /(expr: Expr): Expr = expr match{
    case NumInt(int)=>Rational(1,int)
    case NumDouble(double)=>NumDouble(1.0/double)
    case _=>expr~(-1)
  }

  override def -(expr: Expr): Expr = asInt-expr

  override def *(expr: Expr): Expr = expr

  override def ~(power: Expr): Expr = One

  override def toDouble: Double = 1.0

  override implicit def generality: Generality = Real.generality_top

  override def exprString: String = "1"
}