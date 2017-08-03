package utopia.conflict.util

import utopia.genesis.util.Circle
import utopia.genesis.util.Bounds
import utopia.genesis.util.Vector3D
import utopia.genesis.util.TransformableShape
import utopia.genesis.util.Transformation
import utopia.genesis.util.Angle
import utopia.genesis.util.Extensions._

/**
 * Collision shapes are used when testing collisions between objects. The shapes may consist of 
 * polygons and / or circles. A bounding box is also provided
 * @author Mikko Hilpinen
 * @since 1.8.2017
 * @param convexPolygons The convex polygon (parts) that form this shape, or parts of it. An empty vector if 
 * the shape only consists of circles
 * @param circles The circle(s) that are part of this collision shape. An empty vector if the shape only 
 * consists of a polygon / polygons
 * @param circleToPolygonEdges the amount of edges used when approximating a circle or an ellipsoid 
 * with a polygon
 */
class CollisionShape(val convexPolygons: Vector[Polygon] = Vector(), 
        val circles: Vector[Circle] = Vector(), val circleToPolygonEdges: Int = 12) 
        extends TransformableShape[CollisionShape]
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
    
    /**
     * Converts the shape circle portions to polygons using as many edges as defined in 
     * <i>circleToPolygonEdges</i>
     */
    def circlesAsPolygons = circles.map { circle => Polygon((
                for { i <- 0 until circleToPolygonEdges } yield Vector3D.lenDir(circle.radius, 
                new Angle(math.Pi * 2 * i / circleToPolygonEdges))).toVector) }
    
    
    // IMPLEMENTED METHODS    ----------------
    
    override def transformedWith(transformation: Transformation) = 
    {
        val transformedPolygons = convexPolygons.map { transformation(_) }
        
        // Some transformations allow the circles to retain their shape while others will not
        if ((transformation.shear ~== Vector3D.zero) && (transformation.scaling.x ~== transformation.scaling.y))
        {
            val transformedCircles = circles.map { original => 
                    Circle(transformation(original.origin), original.radius * transformation.scaling.x) }
            new CollisionShape(transformedPolygons, transformedCircles)
        }
        else 
        {
            val transformedCirclePolygons = circlesAsPolygons.map { transformation(_) }
            new CollisionShape(transformedPolygons ++ transformedCirclePolygons)
        }
    }
}