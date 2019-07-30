abstract sealed class MemoryType extends Product with Serializable

/**
 * Variable-length and fixed-length tapes are supported
 */
object MemoryType
{
  final case class Expanding()         extends MemoryType
  final case class Finite(length: Int) extends MemoryType
}