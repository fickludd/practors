package se.lth.immun

import java.io.File
import se.jt.Params

class AAScorerParams extends Params {

	import Params._
	
	val normalize = false ## "normalize properties to the range [-1.0, 1.0]"
	val totals	= false ## "set to compute totals instead of averages"
	
	val aaProp = ReqString("AAIndex formatted file, or .csv with one aa per column")
	val peptides = "" ## "peptide-per-line file to calculate properties for"
	
	def aaFile = new File(aaProp.value)
	def aaOutFile = new File(aaFile.getParent, aaFile.getName + ".csv")
	def pepFile = new File(peptides.value)
	def pepOutFile = 
		new File(stripExt(peptides.value) + ".props.csv")
	
	
	def stripExt(str:String, ext:String) =
		if (str.toLowerCase.endsWith(ext))
			str.dropRight(ext.length)
		else str
	
	def stripExt(str:String) =
		if (str.contains("."))
			str.take(str.lastIndexOf("."))
		else str
}