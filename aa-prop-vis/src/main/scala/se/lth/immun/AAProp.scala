package se.lth.immun

/** 
 * intensities are in this order ARNDCQEGHILKMFPSTWYV
 * only handles stripped nonmodified peps (miscalculation otherwise!)
 */
object AAProp {
	val AAS = "ARNDCQEGHILKMFPSTWYV"
}

case class AAProp(
		key:String,
		name:String,
		ref:String,
		author:String,
		title:String,
		journal:String,
		corrsWith:String,
		ints:Array[Double])