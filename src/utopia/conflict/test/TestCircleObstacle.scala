package utopia.conflict.test

import utopia.genesis.util.Circle
import utopia.conflict.util.CollisionShape
import utopia.conflict.collision.Collidable
import scala.collection.immutable.HashSet
import utopia.conflict.test.TestCollisionGroups.Obstacle
import utopia.genesis.event.Drawable
import utopia.genesis.util.Drawer

/**
 * This is a simple circular obstacle used in visual collision tests
 * @author Mikko Hilpinen
 * @since 6.8.2017
 */
class TestCircleObstacle(val circle: Circle) extends Collidable with Drawable
{
    // IMPLEMENTED PROPERTIES    ---------------
    
    override val collisionShape = CollisionShape(circle)
    
    override val collisionGroups = HashSet(Obstacle)
    
    
    // IMPLEMENTED METHODS    ------------------
    
    override def draw(drawer: Drawer) = drawer.draw(circle)
}