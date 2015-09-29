package se.lth.immun

import scala.swing._
import scala.swing.BorderPanel.Position._


class GUI(params:AAPropVisParams) extends SimpleSwingApplication {
	
	def aaWeighter(aas:Array[Char]) = {
		propList.selection.items.headOption match {
			case Some(prop) =>
				aas.map(aa => prop.ints(AAProp.AAS.indexOf(aa)))
			case None =>
				aas.map(_ => 0.5)
		}
	}
	
	def respFunc(p:PepResponse) = backgroundMode.selection.item.value(p)
	
	val pepMode = new ComboBox(Array(AACenter, AALeft, AARight, AAEnds))
	val backgroundMode = new ComboBox(Array(WaterResp, YeastResp, RespDiff))
	val reorder = new CheckBox { text = "reorder every 5 sec" }
	
	val pepRenderer = new AAPropRenderer(aaWeighter, respFunc)
	
	
	val pepDisplay = new ListView[PepResponse]
	pepDisplay.visibleRowCount = -1
	pepDisplay.peer.setLayoutOrientation(javax.swing.JList.VERTICAL_WRAP)
	pepDisplay.peer.setCellRenderer(pepRenderer)
	
	val propList = new ListView[AAProp]
	propList.renderer = ListView.Renderer(_.name)
	
	val top = new MainFrame {
		title = "AA property visualizer"
		preferredSize = new Dimension(1750, 760)
		
		
		val pepControl = new GridPanel(1, 4) {
			contents += pepMode
			contents += backgroundMode
			contents += reorder
		}
		
		val pepPart = new BorderPanel {
			layout(pepDisplay) = Center
			layout(pepControl) = South
		}
		
		val propColumn = new BorderPanel {
			layout(new ScrollPane(propList)) = Center
			preferredSize = new Dimension(300, 2000)
		}
		
		
		contents = new BorderPanel {
			//layout(gridPanel) = North
			layout(pepPart) = Center
			layout(propColumn) = East
			/*layout(button) = West
		      layout(canvas) = Center
		      layout(toggle) = East
		      layout(textField) = South
		      * 
		      */
		}
	}
	
	if (top.size == new Dimension(0, 0)) top.pack()
	top.visible = true
}