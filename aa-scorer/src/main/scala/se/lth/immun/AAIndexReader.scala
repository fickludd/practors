package se.lth.immun

import java.io.File
import java.io.Reader
import java.io.BufferedReader
import java.io.FileReader
import java.io.IOException

import scala.collection.mutable.ArrayBuffer

class AAIndexReader(normalize:Boolean = false) {
	
	var nextLine:String = _
	
	import AAScorer.AAProp
	
	def read(f:File) = {
		val r = new BufferedReader(new FileReader(f))
		
		val aaProps = new ArrayBuffer[AAProp]
		readLine(r)
		while (nextLine != null && nextLine != "") {
			aaProps += readProp(r)
		}
		r.close()
		
		aaProps
	}
	
	
	private def readLine(r:BufferedReader) = {
		nextLine = r.readLine
	}
	
	private def readCodeLine(r:BufferedReader):(String, String) = {
		val codeLine = nextLine.split(" ", 2)
		readLine(r)
		(codeLine.head, codeLine.last)
	}
		
	def readField(r:BufferedReader, fieldCode:Char):String = {
		val (code, line) = readCodeLine(r)
		if (code.head != fieldCode) 
			throw new Exception("Expected fieldCode '"+fieldCode+ "' got '"+code+"'")
		val sb = new StringBuilder
		sb ++= line
		while (nextLine.head == ' ') {
			sb ++= nextLine
			readLine(r)
		}
		
		sb.result
	}
	
	def normalized01(x:Array[Double]) = {
		val range = x.max - x.min
		x.map(i => (i - x.min) / range)
	}
	
	def normalized11(x:Array[Double]) = {
		val range = x.max - x.min
		x.map(i => (i - x.min - range/2)*2 / range)
	}
	
	def readProp(r:BufferedReader):AAProp = {
		val key 		= readField(r, 'H')
		val propName 	= readField(r, 'D')
		val ref 		= readField(r, 'R')
		val author 		= readField(r, 'A')
		val title 		= readField(r, 'T')
		val journal 	= readField(r, 'J')
		val corrsWith 	= readField(r, 'C')
		val intString 	= readField(r, 'I')
		if (!nextLine.startsWith("//"))
			throw new Exception("Prop '"+key+"' not properly terminated!")
		readLine(r)
		
		val aaLegend = "   A/L     R/K     N/M     D/F     C/P     Q/S     E/T     G/W     H/Y     I/V"
		if (!intString.startsWith(aaLegend))
			throw new Exception("unexpected aa order!")
		val spots = intString.drop(aaLegend.length).split(" ").filter(_ != "")
		val ints = spots.map(x => if (x == "NA") 0 else x.toDouble)
		AAProp(key, propName, ref, author, title, journal, corrsWith, 
				if (normalize) normalized11(ints) else ints)
	}
}