/**
 * Handles the minutiae of "ticking" from one frame to the next.
 */
class Ruleset(rule: Short)
{
  require(rule >= 0 && rule <= 255)
  
  private val rules: Array[Boolean] = Ruleset.numToRule(rule)
  
  /**
   *  Given a cell and its two neighbors, determines the next state of the cell.
   */
  def applyRule(context: Array[Boolean]): Boolean = context match
  {
    case Array(true, true, true)    => rules(0)
    case Array(true, true, false)   => rules(1)
    case Array(true, false, true)   => rules(2)
    case Array(true, false, false)  => rules(3)
    case Array(false, true, true)   => rules(4)
    case Array(false, true, false)  => rules(5)
    case Array(false, false, true)  => rules(6)
    case Array(false, false, false) => rules(7)
    case _              => println("Error: Impossible context!"); false;
  }
    
}

object Ruleset
{
  /**
   * Converts a Wolfram number into the corresponding ruleset.
   */
  def numToRule(n: Short): Array[Boolean] =
  {
    require(n >= 0 && n <= 255)
    
    toBinary(n).map(b => toBool(b)).reverse.padTo(8, false).reverse.toArray
  }
  
  /**
   * Converts a decimal number into its binary equivalent, as a list.
   */
  def toBinary(n: Int): List[Byte] = 
    n.toBinaryString.map(c => c.asDigit.toByte).toList
  
  /**
   * Convenience function to turn a number into a boolean value in the C way.
   */
  def toBool(n: Int): Boolean = if (n == 0) false else true
  
  /**
   * Turns booleans back into integers.
   */
  def fromBool(b: Boolean): Int = if (b) 1 else 0
}