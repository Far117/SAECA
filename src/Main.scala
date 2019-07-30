import scala.io.StdIn
import scala.io.Source
import scala.collection.mutable.ListBuffer

import java.io.File
import java.io.PrintWriter

/**
 * Prompts for the simulation to run, then runs it
 */
object Main 
{
  def main(args: Array[String]) = if (args.length == 0) interactive()
                                  else                  parseArgs(args)
  
  /**
   * Prompts the user for the simulation parameters.
   */
  def interactive() =
  {
    print("Please enter the Wolfram number of the ruleset to simulate:\n> ")
    val rule: Short = StdIn.readShort()
    
    print("Please enter the starting data:\n> ")
    val data: String = StdIn.readLine()
    
    print("How many steps would you like to simulate?\n> ")
    val steps: Int = StdIn.readInt()
    
    print("Would you like the tape to be extensible (y/n)?\n> ")
    val expand: Char = StdIn.readChar()
    
    println("Simulating...")
    
    val memoryType= if (expand == 'y') MemoryType.Expanding()
                    else               MemoryType.Finite(data.length)
    
    val automaton = new Automaton(rule, memoryType, data)
    automaton.stepN(steps)
    val history: List[String] = automaton.getHistory()
    
    println("Done!\n")
    println("Initial tape:")
    println(history(history.length - 1))
    println("Final tape:")
    println(history.head)
    println("\n\nComplete History:")
    
    history.reverse.foreach(println) // History is backwards, chronologically
  }
  
  /**
   * Handles console input when the program is called with simulation parameters.
   */
  def parseArgs(args: Array[String])=
  {
    val split          = splitArgs(args)
    val arguments      = split._1
    val flags          = split._2
    
    if (arguments.length < 4) usage()
    else
    {
      val rule           = arguments(0).toShort
      val steps          = arguments(1).toInt
      val inputFilename  = arguments(2)
      val outputFilename = arguments(3)
      
      
      val source = Source.fromFile(inputFilename)
      val input  = source.getLines.next()
      source.close()
      
      val memoryType = if (flags.contains("-e")) MemoryType.Expanding()
                       else                      MemoryType.Finite(input.length)
      
      val automaton = new Automaton(rule, memoryType, input)
      
      println("Simulating...")
      automaton.stepN(steps)
      println("Done\nWriting output...")
      
      val output = new PrintWriter(new File(outputFilename))
      automaton.getHistory.reverse.foreach(s => output.write(s + "\n"))
      output.close()
      
      println("Done")
    }
  }
  
  /**
   * For arguments given in the form `a1 a2 -f1 -f2 a3 -f3`, returns
   * (["a1", "a2", "a3"], ["-f1", "-f2", "-f3"])
   * 
   * In other words, splits apart arguments and flags, keeping them in
   * the order they appeared.
   */
  def splitArgs(args: Array[String]): (List[String], List[String]) =
  {
    var arguments = new ListBuffer[String]()
    var flags     = new ListBuffer[String]()
    
    args.foreach(s => if (s.charAt(0) == '-') flags += s else arguments += s)
    
    (arguments.toList.reverse, flags.toList.reverse)
  }
  
  /**
   * General usage instructions.
   */
  def usage() =
  {
    println("SAECA: Simulator for Aribitrary Elementary Cellular Automata\n")
    println("Usage: saeca <rule number> <steps to simulate> <input filepath> <output filepath>")
    println("Flags: -e for expanding tape")
  }
}
