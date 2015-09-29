package se.lth.immun

import scala.swing._
import javax.swing.ListCellRenderer
import java.awt.Component
import java.awt.Font
import java.awt.Color
import java.awt.Graphics
import javax.swing.JList
import javax.swing.JLabel
import java.awt.GraphicsEnvironment

trait AAMode
case object AACenter extends AAMode
case object AALeft extends AAMode
case object AARight extends AAMode
case object AAEnds extends AAMode

class AAPropRenderer(
		evalAAs:Array[Char] => Seq[Double],
		getResp:PepResponse => Double
) extends JLabel with ListCellRenderer {
	
	//println(GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.mkString("\n"))
	
	var pep:PepResponse = _
	var col = Color.CYAN.brighter
	setOpaque(true)
	
	var mode:AAMode = AACenter
	
	val PEP_RESP_DIST = 15
	val PEP_RESP_FACTOR = 1.3
	val COL_SEP_WIDTH = 8
	val AA_SIDE_PAD = 15
	
	val BG_COLOR = Color.DARK_GRAY
	
	def getListCellRendererComponent(
			list:JList, 
			a:Any, 
			index:Int,
			isSelected:Boolean, 
			cellHasFocus:Boolean
	):Component = {
		pep = a.asInstanceOf[PepResponse]
		setText(pep.strippedPep)
		//col = if (index % 2 == 0) Color.MAGENTA else Color.GREEN
		this
	}
	
	override def paintComponent(g:Graphics):Unit = {
		val aas = pep.strippedPep.toArray
		val ws = evalAAs(aas)
		g.setFont( new Font("Andale Mono", Font.PLAIN, 12))
		
		val fm = g.getFontMetrics
		val charWidths = aas.map(c => fm.charWidth(c))
		
		val respStr = "%.2f".format(getResp(pep))
		val respW = fm.charsWidth(respStr.toArray, 0, respStr.length)
		val lineY = fm.getAscent-4
		
		def drawAA(i:Int, x:Int) = {
			g.setColor(scale(ws(i).toFloat, col, BG_COLOR))
			g.fillRect(x, 0, charWidths(i), size.getHeight.toInt)
			g.setColor(Color.WHITE)
			g.drawString(aas(i).toString, x, 10)
		}
		
		def drawResp(x:Int, y:Int) = {
			g.setColor(Color.YELLOW.darker)
			val r = (getResp(pep) * PEP_RESP_FACTOR).toInt
			g.fillOval(x - r, y - r, r*2, r*2)
		}
		
		g.setColor(BG_COLOR)
		g.fillRect(COL_SEP_WIDTH, 0, size.getWidth.toInt - COL_SEP_WIDTH*2, size.getHeight.toInt)
		
		g.setColor(Color.DARK_GRAY.darker)
		g.fillRect(0, 0, COL_SEP_WIDTH, size.getHeight.toInt)
		g.fillRect(size.getWidth.toInt - COL_SEP_WIDTH, 0, COL_SEP_WIDTH, size.getHeight.toInt)
		
		
		mode match {
			case AAEnds =>
				var x0 = AA_SIDE_PAD
				for (i <- 0 until ws.length/2) {
					drawAA(i, x0)
					x0 += charWidths(i)
				}
				
				
				val secondHalfX = size.getWidth.toInt - 2*AA_SIDE_PAD - (fm.charsWidth(aas, 0, aas.length) - x0)
				
				drawResp(size.getWidth.toInt / 2, lineY)
				g.drawLine(x0+2, lineY, secondHalfX-2, lineY)
				
				x0 = secondHalfX
				
				for (i <- ws.length/2 until ws.length) {
					drawAA(i, x0)
					x0 += charWidths(i)
				}
				
			case AALeft =>
				drawResp(size.getWidth.toInt - PEP_RESP_DIST, lineY)
				var x0 = AA_SIDE_PAD
				for (i <- 0 until ws.length) {
					drawAA(i, x0)
					x0 += charWidths(i)
				}
				
				
			case AARight =>
				drawResp(PEP_RESP_DIST, lineY)
				var x0 = size.getWidth.toInt - AA_SIDE_PAD - fm.charsWidth(aas, 0, aas.length)
				for (i <- 0 until ws.length) {
					drawAA(i, x0)
					x0 += charWidths(i)
				}
		
				
			case AACenter =>
				drawResp(PEP_RESP_DIST, lineY)
				var x0 = (size.getWidth.toInt - fm.charsWidth(aas, 0, aas.length)) / 2
				for (i <- 0 until ws.length) {
					drawAA(i, x0)
					x0 += charWidths(i)
				}
				
		}
		
		
	}
	
	def scale(w:Float, col:Color, bg:Color) = {
		val cCol = col.getColorComponents(Array(1.0f, 1.0f, 1.0f))
		val cBg = bg.getColorComponents(Array(1.0f, 1.0f, 1.0f))
		new Color(
				cCol(0) * w + cBg(0)* (1 - w),
				cCol(1) * w + cBg(1)* (1 - w),
				cCol(2) * w + cBg(2)* (1 - w))
	}
}