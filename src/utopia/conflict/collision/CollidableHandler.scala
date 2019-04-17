package utopia.conflict.collision

import utopia.conflict.handling.{Collidable, CollisionGroup}
import utopia.inception.handling.HandlerType
import utopia.inception.handling.Handler
import utopia.conflict.util.CollisionShape
import utopia.conflict.util.Collision

case object CollidableHandlerType extends HandlerType(classOf[Collidable])

/**
 * A collidableHandler is used for managing and checking collisions against multiple collidable 
 * isntances. The collidables are grouped in specific subgroups so that collision checks can be 
 * more specific.
 * @author Mikko Hilpinen
 * @since 4.8.2017
 */
class CollidableHandler extends Handler[Collidable](CollidableHandlerType)
{
    /**
     * Finds all collisions (in the specified collision groups) for the specified collision shape
     * @param shape the shape against which the collisions are checked
     * @param limitToGroups None if all of the collidable instance should be checked. A collection 
     * of checked collision groups otherwise
     * @param All collisions in the checked groups along with their collidable participants
     */
    def checkForCollisions(shape: CollisionShape, limitToGroups: Option[Traversable[CollisionGroup]] = None) = 
    {
        val collisionBuffer = Vector.newBuilder[Tuple2[Collidable, Collision]]
        
        foreach(true, collidable => 
        {
            if (limitToGroups.isEmpty || limitToGroups.get.exists { collidable.collisionGroups.contains(_) })
            {
                val collision = shape.checkCollisionWith(collidable.collisionShape)
                if (collision.isDefined)
                {
                    collisionBuffer += collidable -> collision.get
                }
            }
            true
        })
        
        collisionBuffer.result()
    }
}