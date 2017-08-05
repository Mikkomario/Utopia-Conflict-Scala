package utopia.conflict.test

import utopia.genesis.util.Vector3D
import utopia.genesis.util.Line
import utopia.genesis.event.Drawable
import utopia.genesis.util.Drawer
import java.awt.Color
import utopia.genesis.util.Circle
import utopia.conflict.collision.CollisionListener
import utopia.conflict.collision.Collidable
import utopia.conflict.collision.CollisionGroup
import utopia.conflict.util.Collision
import utopia.genesis.util.DepthRange

/**
 * A collision drawer draws collision points and mtv data visually on the screen based on the events 
 * it receives
 * @author Mikko Hilpinen
 * @since 4.8.2017
 */
class CollisionDrawer(target: Collidable, listenGroups: Option[Set[CollisionGroup]] = None) extends Drawable with CollisionListener
{
    // ATTRIBUTES    ---------------------
    
    private var collisionPoints = Vector[Vector3D]()
    private var mtv = Line(Vector3D.zero, Vector3D.zero)
    
    
    // IMPLEMENTED PROPERTIES    --------
    
    override def collisionShape = target.collisionShape
    
    override def targetCollisionGroups = listenGroups
    
    override def depth = DepthRange.foreground
    
    
    // IMPLEMENTED METHODS    -----------
    
    override def draw(drawer: Drawer) = 
    {
        val greenDrawer = drawer.withEdgeColor(Some(Color.GREEN))
        collisionPoints.map { Circle(_, 2) }.foreach(greenDrawer.draw)
        drawer.withEdgeColor(Some(Color.RED)).draw(mtv)
    }
    
    override def onCollision(collisions: Vector[Tuple2[Collidable, Collision]], durationMillis: Double) = 
    {
        println(s"Collides with ${collisions.size} instances")
        
        // Only uses the first collision data
        val collision = collisions.find { _._1 != target }.map { _._2 }
        if (collision.isDefined)
        {
            collisionPoints = collision.get.collisionPoints
            
            if (collisionPoints.isEmpty)
            {
                mtv = Line(Vector3D.zero, Vector3D.zero)
            }
            else
            {
                val mtvStart = Vector3D.average(collisionPoints)
                mtv = Line(mtvStart, mtvStart + collision.get.mtv)
            }
        }
    }
}