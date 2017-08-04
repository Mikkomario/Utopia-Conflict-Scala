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
     * The current shape of the collidable instance.
     */
    def collisionShape: CollisionShape
}