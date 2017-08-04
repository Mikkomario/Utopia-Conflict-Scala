package utopia.conflict.test

import utopia.conflict.util.Polygon
import utopia.conflict.collision.Collidable
import utopia.conflict.util.CollisionShape
import scala.collection.immutable.HashSet
import utopia.conflict.test.TestCollisionGroups.Obstacle
import utopia.genesis.event.Drawable
import utopia.genesis.util.Drawer

/**
 * These are some obstacles that can be used in the tests
 * @author Mikko Hilpinen
 * @since 4.8.2017
 */
class TestPolygonObstacle(private val polygon: Polygon) extends Collidable with Drawable
{
    // IMPLEMENTED PROPERTIES    --------------------
    
    override val collisionShape = CollisionShape(polygon)
    
    override val collisionGroups = HashSet(Obstacle)
    
    
    // IMPLEMENTED METHODS    -----------------------
    
    override def draw(drawer: Drawer) = drawer.draw(polygon)
}