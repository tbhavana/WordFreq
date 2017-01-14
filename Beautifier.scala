import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.util.matching.Regex
object Beautifier  {


	def printList(args: List[_]): Unit = {
		println("In function: getListOfFiles...")
  		var i = 0
  		val writer = new PrintWriter(new File("words.txt"))
  		while(i < args.length){
  			val reader = args(i).toString
  			var readfile : Array[Array[String]] = {
  				Source.fromFile(reader)
  				.getLines().map(_.split("\n").map(_.trim.toString)).toArray
			}
			println(reader)
			var  p:Int =0
			while(p < readfile.length){
				var j = 0
				while(j < readfile(p).length ){
					// Includes space, quotes, doublequotes
					val start = "[.\"\' ]"
					// Includes english alphabets only
					val alphabets = "[A-Za-z]"
					// Includes symbols occuring between text like ' and -
					val intermediate = "['-.]" 
					//
					
					/* Building RegEx Pattern */
					val startplus= "("+start+"+)"
					val alphabetsplus = "("+alphabets+"+)"
					val intermediateplusstar ="("+intermediate+"+)*" 
					val alphabetsstar = "("+alphabets+"*)"
					
					val pattern = new Regex(startplus + alphabetsplus + intermediateplusstar + alphabetsstar)
		        	var firstround:Array[String] = (pattern findAllIn readfile(p)(j)).toArray
		        	val finalpattern = new Regex("([A-Za-z]+)")
		        	var fr:Int =0
		        	while(fr < firstround.length){
		        		var finalround:Array[String] = (finalpattern findAllIn firstround(fr)).toArray
		        		var fnr:Int =0
		        		while(fnr < finalround.length){
		        			writer.write(finalround(fnr).toLowerCase()+" ")
		        			fnr = fnr + 1
		        		}
		        		fr = fr + 1
		        	}
		        	
					
					j = j + 1
				}
				p = p + 1
			}
			i = i + 1
  		}
  		writer.close()
  	}
	def getListOfFiles(dir: String):List[File] = {
		println("In function: getListOfFiles...")
  		val d = new File(dir)
  		if (d.exists && d.isDirectory) {
    		d.listFiles.filter(_.isFile).toList
  		} else {
    		List[File]()
  		}
	}
	
	def main(args: Array[String]): Unit = {
	  val inp = "files/split"
	  println("In function: main")
      var p = getListOfFiles(inp)
      printList(p) 	
	}
}