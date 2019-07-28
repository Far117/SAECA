/**
 * Contains all data associated with the simulation, such as the
 * ruleset, tape, and history of all previous states.
 */
class Automaton(rule: Byte, memoryType: MemoryType, initialData: String)
{
  private var history: List[String] = List.empty
  private val rules = new Ruleset(rule)
  private var tape: List[Byte] = memoryType match
  {
    case MemoryType.Finite(length) => List.empty.padTo(length, 0.toByte)
    case MemoryType.Expanding()    => List.empty
  }
  
  fillString(initialData)
  snapshot()
  
  
  
  /**
   * A wrapper for the below `fill` function which can handle bits in the
   * form of a string.
   */
  def fillString(data: String) = fill(data.map(c => c.asDigit.toByte).toList)
  
  /**
   * Fills the tape with bits to initialize a simulation. Takes care to
   * pad out the tape to the maximum length, assuming the tape is finite.
   */
  def fill(data: List[Byte]) = memoryType match
    {
      case MemoryType.Finite(length) => 
        if (data.length > length)
          println("Error: Tried to fill tape above its capacity!")
        else
          tape = data.padTo(length, 0.toByte)
       
      case MemoryType.Expanding()    => tape = (0.toByte :: data) :+ 0.toByte
    }
  
  /**
   * A convenience function to run the simulation in batches.
   */
  def stepN(n: Int) = for (i <- 0 to n) step()
  
  /**
   * Advances the simulation by one cycle, fully updating the tape to the next
   * state while saving it to the history.
   */
  def step() = memoryType match
    {
      case MemoryType.Finite(_)   => tape = stepFinite(); snapshot()
      case MemoryType.Expanding() => stepExpanding();     snapshot()
    }
  
  /**
   * Splits the tape into staggered chunks of length three which can
   * then be updated according to the ruleset. Note that first and last
   * elements cannot be updated, so they must be reattached to prevent their loss.
   */
  private def stepFinite() = 
    (tape.head :: chunk3(tape).map(c => rules.applyRule(c))) :+ tape.last
  
  /**
   * For variable-length tapes, if the second or second-to-last cell is updated,
   * expand the entire tape by one in both directions to make room.
   */
  private def stepExpanding() =
  {
    val newTape = stepFinite()
    val secondToLast = newTape.length - 2
    if (newTape(1) != tape(1) || newTape(secondToLast) != tape(secondToLast))
      tape = (0.toByte :: newTape) :+ 0.toByte
    else
      tape = newTape
  }
  
  /**
   * Staggers a list into chunks of three. For example:
   * 	[1, 2, 3, 4, 5]
   * Would become:
   * 	[(1, 2, 3), (2, 3, 4), (3, 4, 5)]
   */
  private def chunk3(lst: List[Byte]): List[Array[Byte]] = lst match
  {
    case a :: b :: c :: rst => Array(a, b, c) :: chunk3(b :: c :: rst)
    case _                  => Nil
  }
  
  /**
   * Saves the current state of the tape to the head of the history.
   */
  private def snapshot() = history = tape.mkString("") :: history
  
  /**
   * Returns the history of the simulation run thus far.
   */
  def getHistory(): List[String] = history
  
}