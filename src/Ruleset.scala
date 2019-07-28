/**
 * Handles the minutiae of "ticking" from one frame to the next.
 */
class Ruleset(rule: Byte)
{
  private val rules: Array[Byte] = Ruleset.numToRule(rule)
  
  /**
   *  Given a cell and its two neighbors, determines the next state of the cell.
   */
  def applyRule(context: Array[Byte]): Byte = context match
  {
    case Array(1, 1, 1) => rules(0)
    case Array(1, 1, 0) => rules(1)
    case Array(1, 0, 1) => rules(2)
    case Array(1, 0, 0) => rules(3)
    case Array(0, 1, 1) => rules(4)
    case Array(0, 1, 0) => rules(5)
    case Array(0, 0, 1) => rules(6)
    case Array(0, 0, 0) => rules(7)
    case _              => println("Error: Impossible context!"); 0
  }
    
}

object Ruleset
{
  /**
   * Converts a Wolfram number into the corresponding ruleset.
   */
  def numToRule(n: Byte): Array[Byte] = 
    toBinary(n).reverse.padTo(8, 0.toByte).reverse.toArray
  
  /**
   * Converts a decimal number into its binary equivalent, as a list.
   */
  def toBinary(n: Int): List[Byte] = 
    n.toBinaryString.map(c => c.asDigit.toByte).toList
}