package visualize

import java.awt._
import java.security.InvalidParameterException

import core._
import core.patterns.{PlusGroup, TimesGroup, ExprFactor, ExprGroup}

import scala.collection.mutable


class ExprDrawer(fontBaseSize:Int,fontName:String,fontStyle: Int,graphics2D: Graphics2D) {
  var fontColor = Color.black
  var debugMode = true
  sealed abstract class ExprBox{
    val top: Float
    val bottom: Float
    val width: Float
    final def height = top + bottom
  }
  case class StringBox(text:String,fontSize:Int,width:Float) extends ExprBox{
    override def toString = text
    override val top = fontSize.toFloat/2
    override val bottom = fontSize.toFloat/2
  }
  case class DivideBox(upper:ExprBox,lower:ExprBox) extends ExprBox{
    def barHeight=2f
    override val width = math.max(upper.width,lower.width)*1.1f
    override val top = upper.height+barHeight
    override val bottom = lower.height+barHeight
  }
  case class PowerBox(base:ExprBox,power:ExprBox) extends ExprBox{
    def powerPosRatio = 0.4f
    override val width = base.width+power.width
    override val bottom = base.bottom
    override val top = base.top*powerPosRatio+power.height
  }
  case class PrefixBox(prefix:ExprBox,body:ExprBox) extends ExprBox{
    override val width = prefix.width+body.width
    override val top = math.max(prefix.top,body.top)
    override val bottom = math.max(prefix.bottom,body.bottom)
  }
  case class OperatorBox(Op:String, operands:Seq[ExprBox]) extends ExprBox{
    def opWidth = operands.map(_.height).min
    def opLeadingSpace = opWidth/5
    override val top = operands.map(_.top).max
    override val bottom = operands.map(_.bottom).max
    override val width = operands.map(_.width).sum+ (operands.length-1)*(opWidth+2*opLeadingSpace)
  }
  case class ParenthesesBox(content:ExprBox) extends ExprBox{
    val span = math.max(content.top,content.bottom)
    override val top = span
    override val bottom = span
    val parenthesesWidth = height*0.3f
    override val width = content.width+parenthesesWidth*2
  }

  val baseFont = new Font(fontName,fontStyle,fontBaseSize)

  def fontSizeAtDepth(depth:Int):Int= {
    val ratio:Double = depth match{
      case 0=>1
      case 1=>0.75
      case 2=>0.6
      case 3=>0.5
      case _=>0.4
    }
    (ratio*fontBaseSize).toInt
  }

  def drawExprBox(exprBox: ExprBox,x:Float,y:Float): Unit = {
    graphics2D.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    if(debugMode) {
      graphics2D.setColor(new Color(255, 0, 0, 40))
      graphics2D.fillRect(x.toInt, (y-exprBox.top).toInt, exprBox.width.toInt, exprBox.height.toInt)
    }
    graphics2D.setColor(fontColor)
    exprBox match{
      case strBox @ StringBox(text,fontSize,width) =>
        graphics2D.setFont(new Font(fontName,fontStyle,fontSize))
        graphics2D.drawString(text,x,y+strBox.bottom)

      case divide:DivideBox=>
        val barWidth = divide.width
        def calcX(width:Float):Float= x+(barWidth-width)/2

        val upper: ExprBox = divide.upper
        drawExprBox(upper,calcX(upper.width),y-upper.bottom-divide.barHeight)

        //Draw bar
        graphics2D.setStroke(new BasicStroke(1.5f))
        val barY = (y+2*divide.barHeight).toInt
        graphics2D.drawLine(x.toInt, barY, (x+barWidth).toInt, barY)

        val lower: ExprBox = divide.lower
        drawExprBox(lower,calcX(lower.width),y+divide.barHeight+lower.top)

      case box @ OperatorBox(op,operands) =>
//        def calcY(height:Float):Float = (box.height-height)/2+y
        def drawOp(xPos:Float):Unit = {
          val fontSize = box.opWidth.toInt
          graphics2D.setFont(new Font(fontName,fontStyle,fontSize))
          val yPos = y + fontSize.toFloat/2
          graphics2D.drawString(op,xPos,yPos)
        }

        var currentX = x
        val head = operands.head
        drawExprBox(head,currentX,y)
        currentX += head.width
        for(operand <- operands.tail){
          drawOp(currentX+box.opLeadingSpace)
          currentX += box.opWidth + box.opLeadingSpace
          drawExprBox(operand,currentX,y)
          currentX += operand.width
        }

      case box @ PrefixBox(prefix,body) =>
        drawExprBox(prefix,x,y)
        drawExprBox(body,x+prefix.width,y)

      case box @ ParenthesesBox(content) =>
        val parenthesesFont = new Font(fontName,fontStyle,(box.height*0.8).toInt)
        def drawParentheses(xPos:Float,isLeft:Boolean):Unit={
          graphics2D.setFont(parenthesesFont)
          val op = if(isLeft) "(" else ")"
          val offset = box.height*0.15f
          graphics2D.drawString(op,xPos,y+box.bottom-offset)
        }
        drawParentheses(x,isLeft = true)
        drawExprBox(content,x+box.parenthesesWidth,y)
        drawParentheses(x+content.width+box.parenthesesWidth,isLeft = false)

      case box @ PowerBox(base,power) =>
        drawExprBox(base,x,y)
        val powerY = y - power.bottom
        drawExprBox(power,x+base.width,powerY)
    }
  }

  def wrapInParentheses(needWrap:Boolean)(box:ExprBox):ExprBox = {
    if(needWrap) new ParenthesesBox(box)
    else box
  }

  def stringBox(text: String,fontSize:Int): StringBox = {
    val font = new Font(fontName, fontStyle, fontSize)
    val metrics = graphics2D.getFontMetrics(font)
    new StringBox(text, fontSize, metrics.stringWidth(text))
  }

  def makeExprBox(expr:Expr):ExprBox= {

    def makeBox(expr: Expr, depth: Int): ExprBox = {
      def makeStringBox(text:String):StringBox = stringBox(text,fontSizeAtDepth(depth))


      def buildSeq(op: String, seq: Seq[Expr]): ExprBox = {
        if(seq.length>1) {
          val boxes = op match {
            case "+" => seq.map(e => makeBox(e, depth))
            case "×" => seq.map {
              case plus: Plus => ParenthesesBox(makeBox(plus, depth))
              case e => makeBox(e, depth)
            }
          }
          OperatorBox(op, boxes)
        }else{
          makeBox(seq.head,depth)
        }
      }

      expr match {
        case Symb(name) => makeStringBox(name)
        case r: Real => makeStringBox(r.exprString)
        case Power(b, p) =>
          val baseBox = makeBox(b, depth) match{
            case opBox:OperatorBox => ParenthesesBox(opBox)
            case normal => normal
          }
          new PowerBox(baseBox, makeBox(p, depth + 1))
        case TimesGroup(seq) if seq.length > 1 =>
          def hasMinusPower(expr: Expr) = expr match {
            case ExprFactor(b, NumInt(p)) => p < 0
            case _ => false
          }
          def turnToPosPower(expr: Expr) = expr match {
            case Power(b, p) => b ~ (-1 * p)
            case _=> throw new InvalidParameterException()
          }

          val dropped = seq
          val minusPower = dropped.filter(hasMinusPower).map(turnToPosPower)
          if (minusPower.isEmpty) {
            buildSeq("×", dropped)
          } else {
            val posPower = dropped.filter(!hasMinusPower(_))
            val numerator = if (posPower.isEmpty) makeStringBox("1")
            else buildSeq("×", posPower)

            DivideBox(numerator, buildSeq("×", minusPower))
          }
        case PlusGroup(seq) if seq.length > 1 =>
          buildSeq("+", seq)
        case f1: Func1 =>
          val funcBody: ExprBox = ParenthesesBox(makeBox(f1.arg, depth))
          val funcName: StringBox = makeStringBox(f1.funcName)
          PrefixBox(funcName, funcBody)
      }
    }

    makeBox(expr,0)
  }

}

