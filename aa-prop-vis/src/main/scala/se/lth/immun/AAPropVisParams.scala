package se.lth.immun

import se.jt.Params

class AAPropVisParams extends Params {

	import Params._
	
	val peptides = ReqString("File with peptide sequences to visualize")
	val properties = ReqString("AAIndex style file with amino acid properties")
}