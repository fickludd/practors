package se.lth.immun

object IonBST {

	trait Node {
		var mass:Double = 0.0
		var parent:Option[Branching] = None
		def wMass(mass:Double) = {
			this.mass = mass
			this
		}
	}
	case class Leaf(next:Option[Leaf], id:String, z:Int, prob:Double) extends Node {
		var n:Long = 0
		def wN(n:Long) = {
			this.n = n
			mass = n * prob
			this
		}
		def addOne = {
			n += 1
			mass += prob
			parent.get.bubbleUpdateMass
		}
		def removeOne = {
			n -= 1
			mass -= prob
			parent.get.bubbleUpdateMass
		}
		
		override def toString = "leaf(%s +%d)".format(id, z)
	}
	case class Branching(l:Node, r:Node) extends Node {
		l.parent = Some(this)
		r.parent = Some(this)
		def updateMass =
			mass = l.mass + r.mass
		def bubbleUpdateMass:Unit = {
			updateMass
			parent match {
				case Some(p) => p.bubbleUpdateMass
				case None => {}
			}
		}
	}
	
	
	def update(root:Node, atMass:Double):Leaf = 
		root match {
			case l:Leaf => 
				l.next match {
					case Some(l2) => l2.addOne
					case None => throw new Exception("The last charge state in series should have prob=0: "+l)
				}
				l.removeOne
				l
			case b:Branching =>
				if (atMass > b.l.mass)
					update(b.r, atMass - b.l.mass)
				else
					update(b.l, atMass)
				
	}
	
	def ionSeries(id:String)(ppiz:((Double, Long), Int), next:Option[Leaf]):Option[Leaf] = {
		val ((prob, n), z) = ppiz
		Some(Leaf(next, id, z, prob).wN(n))
	}
	def unroll(l:Leaf):List[Node] =
		l.next match {
			case None => l :: Nil
			case Some(l2) => l :: unroll(l2)
		}
		 
	def construct(ions:Array[(String, Array[(Double, Long)])]) = {
		
		val leafSeries = ions.map(ion => {
			val zs = ion._2.zipWithIndex
			zs.foldRight[Option[Leaf]](None)(ionSeries(ion._1)).get
		})
		
		branch(leafSeries.flatMap(ls => unroll(ls)))
	}
	
	def branch(nodes:Seq[Node]):Node = {
		nodes.length match {
			case 0 =>
				throw new Exception("Unknown behaviour for empty node-list")
			case 1 =>
				nodes.head
			case n =>
				val l = branch(nodes.take(n/2))
				val r = branch(nodes.drop(n/2))
				val b = Branching(l, r)
				b.updateMass
				b
		}
	}
	
	def leaves(root:Node):List[Leaf] = 
		root match {
			case l:Leaf =>
				l :: Nil
			case Branching(l, r) =>
				leaves(l) ::: leaves(r)
		}
}