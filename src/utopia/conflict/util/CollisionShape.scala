package utopia.conflict.util

import utopia.genesis.util.Circle
import utopia.genesis.util.Bounds
import utopia.genesis.util.Vector3D

/**
 * Collision shapes are used when testing collisions between objects. The shapes may consist of 
 * polygons and / or circles. A bounding box is also provided
 * @author Mikko Hilpinen
 * @since 1.8.2017
 * @param convexPolygons The convex polygon (parts) that form this shape, or parts of it. An empty vector if 
 * the shape only consists of circles
 * @param circles The circle(s) that are part of this collision shape. An empty vector if the shape only 
 * consists of a polygon / polygons
 */
class CollisionShape(val convexPolygons: Vector[Polygon] = Vector(), 
        val circles: Vector[Circle] = Vector())
{
    // ATTRIBUTES    -------------------------
    
    /**
     * A bounding box around the whole shape. It is recommended that a bounding box is checked 
     * for multipart shapes when performing complex operations.
     */
    lazy val bounds = Bounds.around(convexPolygons.map { _.bounds } ++ circles.map(Bounds.around)
            ).getOrElse(Bounds(Vector3D.zero, Vector3D.zero))
    
    
    // COMPUTED PROPERTIES    ----------------
    
    /**
     * Whether this shape consists of more than a single part. A bounding box should be used for 
     * multipart shapes
     */
    def isMultiPart = circles.size + convexPolygons.size > 1
}