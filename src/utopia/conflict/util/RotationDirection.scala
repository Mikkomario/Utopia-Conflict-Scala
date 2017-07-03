package utopia.conflict.util

sealed trait RotationDirection

/**
 * This object introduces the two directions rotation can be done. They can be used like enumerations. 
 * @author Mikko Hilpinen
 * @since 29.6.2017
 */
object RotationDirection
{
    // VALUES    ----------------------
    
    /**
     * The clockwise rotation direction, which is the default positive rotation direction in 
     * Java, Scala and Utopia too
     */
    case object Clockwise extends RotationDirection
    
    /**
     * The counter-clockwise rotation direction, which is considered the negative direction
     */
    case object CounterClockwise extends RotationDirection
    
    
    // OPERATORS    ------------------
    
    /**
     * Finds the appropriate rotation direction to describe the provided rotation amount. Positive 
     * rotation is clockwise while negative is counter-clockwise
     */
    def apply(rotation: Double): RotationDirection = if (rotation < 0) CounterClockwise else Clockwise
}