package utopia.conflict.collision

import utopia.inception.handling.Handleable
import utopia.conflict.util.CollisionShape
import utopia.genesis.util.Transformation

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
        
    }*/
}