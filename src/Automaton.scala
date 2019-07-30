import scala.collection.mutable.ListBuffer

/**
 * Contains all data associated with the simulation, such as the
 * ruleset, tape, and history of all previous states.
 */
class Automaton(rule: Short, memoryType: MemoryType, initialData: String)
{
  require(rule >= 0 && rule <= 255)
  
  private var history = new ListBuffer[String]()
  private val rules = new Ruleset(rule)
  private var tape: List[Boolean] = memoryType match
  {
    case MemoryType.Finite(length) => List.empty.padTo(length, false)
    case MemoryType.Expanding()    => List.empty
  }
  
  fillString(initialData)
  snapshot()
  
  
  
  /**
   * A wrapper for the below `fill` function which can handle bits in the
   * form of a string.
   */
  def fillString(data: String) = 
    fill(data.map(c => Ruleset.toBool(c.asDigit)).toList)
  
  /**
   * Fills the tape with bits to initialize a simulation. Takes care to
   * pad out the tape to the maximum length, assuming the tape is finite.
   */
  def fill(data: List[Boolean]) = memoryType match
  {
    case MemoryType.Finite(length) => 
      if (data.length > length)
        println("Error: Tried to fill tape above its capacity!")
      else
        tape = data.padTo(length, false)
     
    case MemoryType.Expanding()    => tape = (false :: data) :+ false
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
   * then be updated according to the ruleset. The first and last elements
   * are analyzed as if they were touching, essentially creating a tape
   * which is a large circle.
   */
  private def stepFinite() = 
    (leftmostCell :: chunk3(tape).map(c => rules.applyRule(c))) :+ rightmostCell
  
  /**
   * For variable-length tapes, if the head or tail of the tape is modified,
   * increases the size by one in both directions.
   */
  private def stepExpanding() =
  {
    val newTape = stepFinite()
    if (newTape.head != tape.head || newTape.last != tape.last)
      tape = (false :: newTape) :+ false
    else
      tape = newTape
  }
  
  /**
   * For a tape in the form of [a, b, ..., c], applies the rule to
   * (c, a, b), simulating the leftmost cell as if the tape were cyclical.
   */
  private def leftmostCell(): Boolean = 
    rules.applyRule(Array(tape.last, tape.head, tape(1)))
  
  /**
   * For a tape in the form of [a, ..., b, c], applies the rule to
   * (b, c, a), simulating the rightmost cell as if the tape were cyclical.
   */
  private def rightmostCell(): Boolean =
  {
    val secondToLast = tape.length - 2
    rules.applyRule(Array(tape(secondToLast), tape.last, tape.head))
  }
  
  /**
   * Staggers a list into chunks of three. For example:
   * 	[1, 2, 3, 4, 5]
   * Would become:
   * 	[(1, 2, 3), (2, 3, 4), (3, 4, 5)]
   */
  private def chunk3(lst: List[Boolean]): List[Array[Boolean]] = lst match
  {
    case a :: b :: c :: rst => Array(a, b, c) :: chunk3(b :: c :: rst)
    case _                  => Nil
  }
  
  /**
   * Saves the current state of the tape to the head of the history.
   */
  private def snapshot() = 
    history += prettify(tape.map(b => Ruleset.fromBool(b)).mkString(""))
  
  /**
   * Returns the history of the simulation run thus far.
   */
  def getHistory(): List[String] = 
  {
    val lst    = history.toList
    val maxLen = lst.map(_.length).max
    val padded = lst.map(s => biPadTo(s, '\u2610', maxLen))
    padded.reverse
  }

  /**
   * Turns 1s into black boxes, and 0s into white boxes.
   */
  def prettify(str: String): String = 
    str.map(c => if (c == '1') '\u25A0' else '\u2610')
  
  /**
   * Similar to `padTo`, except it appends an element to the front and end
   * of a list each time, instead of just the end. Will pad up to either
   * `len` or `len + 1` elements.
   */
  def biPadTo(inp: String, chr: Char, len: Int): String = 
    if (inp.length < len) biPadTo(chr + inp + chr, chr, len)
    else                  inp
  
}