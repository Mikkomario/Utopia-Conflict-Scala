package utopia.conflict.util

import utopia.genesis.util.Bounds
import scala.language.implicitConversions
import utopia.genesis.util.Circle
import utopia.genesis.util.Vector3D
import utopia.genesis.util.Angle

/**
 * This object contains extensions that are used in the conflict project
 * @author Mikko Hilpinen
 * @since 13.7.2017
 */
object Extensions
{
    // Bounds can be represented as polygons where necessary
    implicit def boundsToPolygon(bounds: Bounds) = Polygon(bounds.corners2D)
    
    implicit class CollisionCircle(val c: Circle) extends AnyVal
    {
        /**
         * Checks if there's collision between two circles. Returns collision data if there is 
         * a collision.
         */
        def checkCollisionWith(other: Circle) = 
        {
            val mtv = c.collisionMtvWith(other)
            mtv.map { new Collision(_, c.circleIntersection(other)) }
        }
        
        /**
         * Converts this circle into a polygon with the specified amount of edges. The higher the 
         * amount of edges, the more accurate the representation but the more taxing any operations 
         * will be.
         */
        def toPolygon(edgeAmount: Int) = Polygon((
                for { i <- 0 until edgeAmount } yield Vector3D.lenDir(c.radius, 
                new Angle(math.Pi * 2 * i / edgeAmount))).toVector)
    }
}