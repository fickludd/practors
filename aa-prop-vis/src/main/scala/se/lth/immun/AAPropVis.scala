package se.lth.immun

import se.jt.CLIApp
import java.util.Properties

import akka.actor._


object AAPropVis extends CLIApp {

	val params = new AAPropVisParams
	
	var properties = new Properties
	properties.load(this.getClass.getResourceAsStream("/pom.properties"))
	val name 		= properties.getProperty("pom.artifactId")
	val version 	= properties.getProperty("pom.version")
	
	
	def main(args:Array[String]):Unit = {
		failOnError(parseArgs(name, version, args, params, List("peptides", "properties"), None))
		
		val system = ActorSystem()
		
		val swing = system.actorOf(
				SwingActor.props(params).withDispatcher("swing-dispatcher"), 
				"swing-actor"
			)
	}
	
}