/*
 * IonCompeter
 *	Copyright (C) 2014 Johan Teleman
 */
package se.lth.immun

import java.util.Properties
import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * File format 
 * tab separated with header
 * Need to have complete charge series for each id, starting from 0 
 * 
 * ID				charge state	+1 charge likelihood	n molecules	
 * (peptide seq)
 */
object IonCompeter {

	case class IonSpecies(id:String, charge:Int, prob:Double, n:Long)
	class ProbPop(val prob:Double, var n:Long)
	
	val properties = new Properties
	properties.load(this.getClass.getResourceAsStream("/pom.properties"))

	def main(args:Array[String]):Unit = {
		println(properties.getProperty("pom.name")+" "+
							properties.getProperty("pom.version"))
		
		if (args.length != 2) {
			println("usage:\nion-competer <inFile> <iterations>")
			System.exit(1)
		}
		
		val iterations = args(1).toInt
		val in = new File(args(0))
		val inReader = new BufferedReader(new FileReader(in))
		val header = inReader.readLine()
		var line = inReader.readLine()
		
		val ions = new ArrayBuffer[IonSpecies]
		while (line != null) {
			val vals = line.split('\t')
			ions += IonSpecies(vals(0), vals(1).toInt, vals(2).toDouble, vals(3).toInt)
			line = inReader.readLine()
		}
		
		println("loaded in file, total charge: "+ions.map(ion => ion.n * ion.charge).sum)
		val t0 = java.lang.System.currentTimeMillis()
		
		val ids = ions.map(_.id).toSet
		val data = ids.map(id => (id, ions.filter(is => is.id == id).sortBy(_.charge).map(is => new ProbPop(is.prob, is.n)).toArray)).toArray
		
		//simpleSolve(data, iterations)
		//val outIons = data.flatMap(t => t._2.zipWithIndex.map(ppi => IonSpecies(t._1, ppi._2, ppi._1.prob, ppi._1.n)))
		
		val outIons = bstSolve(data, iterations)
		
		println
		println("calculation completed")
		
		println("computed out file, total charge: "+outIons.map(ion => ion.n * ion.charge).sum)
		
		val t = java.lang.System.currentTimeMillis()
		println("wall time: " + formatMillis(t - t0))
		
		val writer = new BufferedWriter(new FileWriter(new File("out.tsv")))
		writer.write(header + "\n")
		for (ion <- outIons)
			writer.write(Array(ion.id, ion.charge.toString, ion.prob.toString, ion.n.toString).mkString("\t")+"\n")
		writer.close()
	}
	
	def simpleSolve(data:Array[(String, Array[ProbPop])], iterations:Int) = {
		for (i <- 0 until iterations) {
			val ps = data.map(t => (t._1, t._2.map(pp => pp.prob*pp.n)))
			val r = Random.nextDouble * ps.map(t => t._2.sum).sum
			var acc = 0.0
			var j = 0
			var k = 0	
			while (acc < r && j < data.length) {
				k = 0
				while (acc < r && k < data(j)._2.length) {
					acc += ps(j)._2(k)					
					k += 1
				}
				j += 1
			}
			data(j-1)._2(k-1).n -= 1
			data(j-1)._2(k).n += 1
			//if (i % 100 == 0)
			//	print(".")
		}
	}
	
	def bstSolve(data:Array[(String, Array[ProbPop])], iterations:Int) = {
		val bst = IonBST.construct(data.map(t => (t._1, t._2.map(pp => (pp.prob, pp.n)))))
		for (i <- 0 until iterations) {
			val r = bst.mass * Random.nextDouble
			IonBST.update(bst, r)
		}
		IonBST.leaves(bst).map(l => IonSpecies(l.id, l.z, l.prob, l.n))
	}
	
	def formatMillis(t:Long) = {
		val s = t / 1000
		val m = s / 60
		val h = m / 60
		val d = h / 24
		"%d days %02d:%02d:%02d.%03d".format(d, h % 24, m % 60, s % 60, t % 1000)
	}
}
