import scala.io.StdIn

/*
 * Prompts for the simulation to run, then runs it
 */
object Main 
{
  def main(args: Array[String]) =
    {
      print("Please enter the Wolfram number of the ruleset to simulate:\n> ")
      val rule: Byte = StdIn.readByte()
      
      print("Please enter the starting data:\n> ")
      val data: String = StdIn.readLine()
      
      print("How many steps would you like to simulate?\n> ")
      val steps: Int = StdIn.readInt()
      
      println("Simulating...")
      
      val automaton = new Automaton(rule, MemoryType.Finite(data.length), data)
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
}
