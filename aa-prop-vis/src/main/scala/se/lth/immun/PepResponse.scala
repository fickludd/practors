package se.lth.immun

trait RespBackground { def value(p:PepResponse):Double }
case object WaterResp extends RespBackground { def value(p:PepResponse) = p.respWater }
case object YeastResp extends RespBackground { def value(p:PepResponse) = p.respYeast }
case object RespDiff extends RespBackground { def value(p:PepResponse) = math.abs(p.respWater - p.respYeast) }

case class PepResponse(
		strippedPep:String,
		mz:Double,
		respWater:Double,
		respYeast:Double
	)
