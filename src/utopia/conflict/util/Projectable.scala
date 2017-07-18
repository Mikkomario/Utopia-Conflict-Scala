package utopia.conflict.util

import utopia.genesis.util.Vector3D
import utopia.genesis.util.Line

/**
 * This trait is extended by shapes that can be projected over a specified axis
 * @author Mikko Hilpinen
 * @since 18.7.2017
 */
trait Projectable
{
    // ABSTRACT METHODS    --------------------
    
    /**
     * Projects this shape, creating a line parallel to the provided axis
     */
    def projectedOver(axis: Vector3D): Line
    
    
    // OTHER METHODS    -----------------------
    
    // Calculates if / how much the projections of the two shapes overlap on the specified axis
    // Returns the mtv for the specified axis, if there is overlap
    def projectionOverlapWith(other: Projectable, axis: Vector3D) = 
    {
        val projection = projectedOver(axis)
        val otherProjection = other.projectedOver(axis)
        
        if (projection.end < otherProjection.start || projection.start > otherProjection.end)
        {
            None
        }
        else 
        {
            val forwardsMtv = otherProjection.end - projection.start
            val backwardsMtv = otherProjection.start - projection.end
            
            if (forwardsMtv.length < backwardsMtv.length) Some(forwardsMtv) else Some(backwardsMtv)
        }
    }
}