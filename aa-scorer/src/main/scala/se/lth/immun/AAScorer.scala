/*
 * AAScorer
 *	Copyright (C) 2014 Johan Teleman
 */
package se.lth.immun

import java.util.Properties
import java.io.File
import java.io.Reader
import java.io.BufferedReader
import java.io.FileReader
import java.io.BufferedWriter
import java.io.FileWriter

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

import se.jt.CLIApp

object AAScorer extends CLIApp {
	
	/** 
	 * intensities are in this order ARNDCQEGHILKMFPSTWYV
	 * only handles stripped nonmodified peps (miscalculation otherwise!)
	 */
	case class AAProp(
			key:String,
			name:String,
			ref:String,
			author:String,
			title:String,
			journal:String,
			corrsWith:String,
			ints:Array[Double])

	val PROP_HEADER = "prop\tA\tR\tN\tD\tC\tQ\tE\tG\tH\tI\tL\tK\tM\tF\tP\tS\tT\tW\tY\tV"
			
	val properties = new Properties
	properties.load(this.getClass.getResourceAsStream("/pom.properties"))

	def main(args:Array[String]):Unit = {
		
		val name = properties.getProperty("pom.name")
		val version = properties.getProperty("pom.version")
		
		println("+++ "+	name + " " + version)
		
		val params = new AAScorerParams
		
		failOnError(parseArgs(name, version, args, params, List("aaProp"), Some("peptides")))
		
		val aaProps = 
			readAAProps(params.aaFile, params.normalize)
		
		if (params.peptides.value == "") {
			val outFile = params.aaOutFile
			val w = new BufferedWriter(new FileWriter(outFile))
			w.write(PROP_HEADER+"\n")
			for (prop <- aaProps) {
				w.write(prop.name + "\t" + prop.ints.mkString("\t") + "\n")
			}
			w.close()
		} else {
			
			println("WARNING: only nonmodified stripped peps supported, miscalculation otherwise!")
			
			val r = new BufferedReader(new FileReader(params.pepFile))
			var pep = r.readLine
			val peps = new ArrayBuffer[String]
			while (pep != null) {
				peps += pep
				pep = r.readLine
			}
			r.close()
			
			val aas = "ARNDCQEGHILKMFPSTWYV"
			val outFile = params.pepOutFile
			val w = new BufferedWriter(new FileWriter(outFile))
			w.write("pep\t%s\n".format(
					aaProps.flatMap(aa => 
						if (params.totals)
							Array("\"Total "+aa.name+"\"")
						else
							Array("\"Average "+aa.name+"\"")
					).mkString("\t")
				))
			for (pep <- peps) {
				val counts = aas.map(aa => pep.count(_ == aa))
				val sumAvgs = for (prop <- aaProps) yield {
					val sum = prop.ints.zip(counts).map(t => t._1 * t._2).sum
					if (params.totals) Array(sum) else Array(sum / pep.length)
				}
				w.write((pep +: sumAvgs.flatten.map(_.toString)).mkString("\t")+"\n")
			}
			w.close()
			
		}
	}
	
	
	def readAAProps(f:File, normalize:Boolean):Seq[AAProp] = {
		try {
			new AAIndexReader(normalize).read(f)
		} catch {
			case e:Exception =>
				try {
					readAAPropsCsv(f)
				} catch {
					case e2:Exception =>
						e.printStackTrace
						e2.printStackTrace
						throw new Exception("Failed reading AA props. quitting....")
				}
		}
	}
	
	def readAAPropsCsv(f:File):Seq[AAProp] = {
		val r = new BufferedReader(new FileReader(f))
		
		val aaProps = new ArrayBuffer[AAProp]
		val header = r.readLine
		if (!header.startsWith(PROP_HEADER))
			throw new Exception("Cannot read '%s' as a AA properties csv file.".format(f.toString))
		var line = r.readLine
		while (line != null && line != "") {
			val parts = line.split("\t")
			aaProps += AAProp("", parts.head, "", "", "", "", "", parts.tail.map(_.trim.toDouble))
			line = r.readLine
		}
		r.close
		
		aaProps
	}
}
