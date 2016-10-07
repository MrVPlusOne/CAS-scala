package visualize

import java.awt.Color
import java.awt.image.BufferedImage

import core._
import core.operation.{SortExpr, Simplify, DiffInfo, Diff}
import parser.Parser

import scala.swing._

object PlotProgram extends SimpleSwingApplication{

  val List(x,y,z,a,b) = List("x","y","z","a","b").map(x=>Symb(x))
  val expr = Parser.parseExpr("b/(1+a^(y+2*y))").get
  val derivative = SortExpr{Diff(new DiffInfo(symbol = "y")){expr}}
//  val expr = Sin(Symb("a")/"Hi There"+"And")~(NumInt(3)/"a")+Sin("x")
  override def top: Frame = new MainFrame(){
    title = MathString{expr}
    preferredSize=new Dimension(400,300)
    contents = new BoxPanel(Orientation.Horizontal){
      background = Color.white

      def draw(g: Graphics2D): Unit ={
        val font = g.getFont
        val drawer = new ExprDrawer(fontBaseSize = 15,"sans-serif",font.getStyle,g){
          debugMode = false
        }
        val box=drawer.makeExprBox(expr)
        val h = box.height
        drawer.drawExprBox(box,30,30)
        drawer.drawExprBox(drawer.makeExprBox(derivative),30,100)
//        println(s"${box},height = ${h}}")
      }

      override def paint(g: Graphics2D): Unit = {
        super.paint(g)
        draw(g)
      }
    }
  }
}
