package se.lth.immun

import akka.actor._
import scala.swing._
import scala.swing.event._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

import scala.io.Source
import scala.collection.mutable.ArrayBuffer


object SwingActor {
	def props(params:AAPropVisParams) =
		Props(classOf[SwingActor], params)
}

case object RandomReorder {}

class SwingActor(params:AAPropVisParams) extends Actor with Reactor {

	val gui = new GUI(params)
	
	val props = AAIndexReader.readAAProps(params.properties, true)
	val peps = readPeptides(params.peptides)
	
	gui.pepDisplay.listData = peps.sortBy(_.respWater).reverse
	gui.propList.listData = props
	
	listenTo(gui.propList.selection)
	listenTo(gui.pepMode.selection)
	listenTo(gui.backgroundMode.selection)
	listenTo(gui.reorder)
	
	def receive = {
		case RandomReorder =>
			if (gui.reorder.selected) {
				gui.pepDisplay.listData = reorder(peps)
				scheduleReorder
			}
			
		case x => println(x)
	}
	
	reactions += {
		case sc:SelectionChanged if sc.source == gui.propList =>
			gui.pepDisplay.repaint
			
		case sc:SelectionChanged if sc.source == gui.pepMode =>
			gui.pepRenderer.mode = gui.pepMode.selection.item
			gui.pepDisplay.repaint
			
		case sc:SelectionChanged if sc.source == gui.backgroundMode =>
			gui.pepDisplay.listData = reorder(peps)
				
			
		case e:ButtonClicked if e.source == gui.reorder =>
			if (gui.reorder.selected) 
				scheduleReorder
	}
	
	
	def scheduleReorder = 
		context.system.scheduler.scheduleOnce(5 seconds) { self ! RandomReorder }
	
	
	def reorder(peps:Seq[PepResponse]):Seq[PepResponse] = 
		peps.sortBy(pep => gui.backgroundMode.selection.item.value(pep)  * (Random.nextDouble + 4)/5).reverse
	
	
	
	def readPeptides(path:String) = {
		var headerParsed = false
		var res = new ArrayBuffer[PepResponse]
		for (line <- Source.fromFile(path).getLines) {
			val cols = line.split("\t")map(_.trim)
			if (!headerParsed) {
				headerParsed = true
			} else {
				res += PepResponse(cols(0), cols(1).toDouble, cols(2).toDouble, cols(3).toDouble)
			}
		}
		res
	}
}