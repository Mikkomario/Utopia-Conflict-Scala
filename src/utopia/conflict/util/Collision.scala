package utopia.conflict.util

import utopia.genesis.util.Vector3D
import utopia.genesis.util.TransformableShape
import utopia.genesis.util.Transformation

/**
 * Collision instances contain information about a collision event
 * @author Mikko Hilpinen
 * @since 13.7.2017
 * @param mtv The minimum translation vector for the primary collision participant. A minimum translation 
 * vector defines the smallest possible translation that gets the participant out of the 
 * collision situation
 * @param calculateCollisionPoints This function returns the collision / intersection points 
 * involved in the collision event. The function is called when the points are requested for the 
 * first time
 */
class Collision(val mtv: Vector3D, calculateCollisionPoints: => Vector[Vector3D]) extends 
        TransformableShape[Collision]
{   
    // ATTRIBUTES    ---------------------
    
    /**
     * The points where the two collision participants intersect
     */
    lazy val collisionPoints = calculateCollisionPoints
    
    
    // OPERATORS    ----------------------
    
    /**
     * This collision from the opposite point of view
     */
    def unary_- = new Collision(-mtv, collisionPoints)
    
    /**
     * Combines two collision information instances
     */
    def +(other: Collision) = new Collision(mtv + other.mtv, (collisionPoints ++ other.collisionPoints).distinct)
    
    
    // IMPLEMENTED METHODS    ------------
    
    override def transformedWith(transformation: Transformation) = new Collision(
            transformation(mtv), collisionPoints.map { transformation(_) })
}