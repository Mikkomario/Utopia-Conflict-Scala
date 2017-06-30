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
    
    
    // OTHER METHODS    --------------
    
    /**
     * Finds the rotation direction that takes a 0 radians direction and to the specified direction 
     * with less effort. Eg. Pi/2 would be clockwise as 3*Pi/2 would be counter-clockwise.
     */
    def ofRadians(radians: Double): RotationDirection = 
    {
        val downscaled = radians % (2 * math.Pi)
        val absolute = if (downscaled < 0) downscaled + 2 * math.Pi else downscaled
        if (absolute <= math.Pi) Clockwise else CounterClockwise
    }
    
    /**
     * Finds the rotation direction that takes a 0 degrees direction and to the specified direction 
     * with less effort. Eg. 90 degrees would be clockwise as 270 degrees would be counter-clockwise.
     */
    def ofDegrees(degrees: Double) = ofRadians(degrees.toRadians)
}