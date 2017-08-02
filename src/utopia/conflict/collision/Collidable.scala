package utopia.conflict.collision

import utopia.inception.handling.Handleable
import utopia.conflict.util.CollisionShape
import utopia.genesis.util.Transformation
import utopia.conflict.util.Extensions._

/**
 * Collidable instances can be collided with, they have a specific collision shape
 * @author Mikko Hilpinen
 * @since 2.8.2017
 */
trait Collidable extends Handleable
{
    // ABSTRACT METHODS / PROPERTIES    --------------------------
    
    /**
     * The shape of this instance, without transformations applied
     */
    def collisionShape: CollisionShape
    
    /**
     * The current transformation state of this instance. This will affect how the collisionShape 
     * is projected to the absolute world-space
     */
    def transformation: Transformation
    
    
    // OTHER METHODS    -----------------------------------------
    
    /*
    def checkCollisionWith(other: Collidable)
    {
        // If one of the shapes is complex, checks collision between bounding boxes first
        if (collisionShape.isMultiPart || other.collisionShape.isMultiPart)
        {
            // Collision between bounding boxes -> check continues
            if (transformation(collisionShape.bounds).checkCollisionWith(
                    other.transformation(other.collisionShape.bounds)).isDefined)
            {
                // First checks collisions between polygon parts, where applicable
                val myPolygons = collisionShape.convexPolygons.map{ transformation(_) }
                val otherPolygons = other.collisionShape.convexPolygons.map { other.transformation(_) }
                
                val polygonCollision = myPolygons.flatMap { myPolygon => otherPolygons.flatMap 
                        { myPolygon.checkCollisionWith(_) } }.reduceOption { _ + _ }
                        
                if (polygonCollision.isDefined)
                {
                    polygonCollision
                }
                else
                {
                    // Next checks between circles, where applicable
                    // Challenge: Circles can't always be transformed while keeping their circle shape
                }
            }
        }
    }
    * */
}