package utopia.conflict.test

import utopia.conflict.util.Polygon
import utopia.genesis.util.Transformation
import utopia.conflict.util.CollisionShape
import utopia.conflict.collision.Collidable
import scala.collection.immutable.HashSet
import utopia.conflict.test.TestCollisionGroups.Obstacle
import utopia.conflict.test.TestCollisionGroups.UserInput
import utopia.genesis.event.Drawable
import utopia.genesis.util.Drawer
import utopia.genesis.event.MouseMoveListener
import utopia.genesis.event.MouseMoveEvent

/**
 * This polygon-shaped obstacle moves along with the mouse
 * @author Mikko Hilpinen
 * @since 4.8.2017
 */
class MousePolygonObstacle(private val relativePolygon: Polygon) extends Collidable with Drawable with MouseMoveListener
{
    // ATTRIBUTES    ------------------
    
    private val relativeCollisionShape = CollisionShape(relativePolygon)
    private var currentTransformation = Transformation.identity
    
    
    // IMPLEMENTED PROPERTIES    -----
    
    override def collisionShape = currentTransformation.toAbsolute(relativeCollisionShape)
    
    override def collisionGroups = HashSet(UserInput)
    
    
    // IMPLEMENTED METHODS    --------
    
    override def draw(drawer: Drawer) = drawer.transformed(currentTransformation).draw(relativePolygon)
    
    override def onMouseMove(event: MouseMoveEvent) = currentTransformation = Transformation.translation(event.mousePosition)
}