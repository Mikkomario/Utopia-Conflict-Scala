package utopia.conflict.util

import utopia.genesis.util.Vector3D
import utopia.genesis.util.Bounds
import utopia.genesis.util.Line
import scala.collection.mutable.ListBuffer
import utopia.genesis.util.Extensions._
import utopia.genesis.util.Circle
import utopia.genesis.util.Area
import utopia.genesis.util.ShapeConvertible
import utopia.genesis.util.TransformableShape
import utopia.genesis.util.Transformation
import utopia.genesis.util.Projectable
import utopia.conflict.util.Extensions._

object Polygon
{
    // Calculates collision points between two polygon edges by using clipping
    // ReferenceNormal is the collision normal (mtv) that from the collision area towards the 
    // reference polygon
    private def clipCollisionPoints(reference: Line, incident: Line, referenceNormal: Vector3D) = 
    {
        // First clips the incident edge from both sides
        val clipped = incident.clipped(reference.start, reference.vector).flatMap 
                { _.clipped(reference.end, -reference.vector) }
        
        if (clipped.isDefined)
        {
            // Also removes any points past the third side
            val origin = reference.start dot referenceNormal
            val startDistance = clipped.get.start.dot(referenceNormal) - origin
            val endDistance = clipped.get.end.dot(referenceNormal) - origin
            
            if (startDistance < 0 && endDistance < 0)
            {
                Vector()
            }
            else if (startDistance < 0)
            {
                Vector(clipped.get.end)
            }
            else if (endDistance < 0)
            {
                Vector(clipped.get.start)
            }
            else
            {
                Vector(clipped.get.start, clipped.get.end)
            }
        }
        else
        {
            Vector()
        }
    }
}

/**
 * Polygons are used for representing more complicated shapes. Polygons only support 2D shapes on 
 * the x-y -plane
 * @author Mikko Hilpinen
 * @since 25.6.2017
 */
case class Polygon(val vertices: Vector[Vector3D]) extends Area with ShapeConvertible with 
        TransformableShape[Polygon] with Projectable
{
    // ATTRIBUTES    ------------------
    
    /**
     * The top left corner of a bounding box around this polygon
     */
    lazy val min = Vector3D.min(vertices).getOrElse(Vector3D.zero)
    
    /**
     * The bottom right corner of a bounding box around this polygon
     */
    lazy val max = Vector3D.max(vertices).getOrElse(Vector3D.zero)
    
    /**
     * A bounding box around this polygon. All of the polygon's points lay inside its bounds
     */
    lazy val bounds = Bounds.between(min, max)
    
    /**
     * The edges that form the sides of this polygon. A polygon of two vertices contains only a 
     * single edge while larger polygons contain same amount of edges as vertices.
     */
    lazy val edges = if (size == 2) Vector(edge(0)) else for { i <- 0 until size } yield edge(i)
    
    /**
     * The order of the vertices in the polygon. The polygons either form the shape in clockwise 
     * or counterclockwise order
     */
    lazy val rotationDirection = RotationDirection(rotations.reduce { _ + _ })
    
    /**
     * Whether the polygon is convex. Convex polygons only need to turn clockwise or 
     * counter-clockwise when traversing through the polygon. They don't have spikes or holes, 
     * so to speak.
     */
    lazy val isConvex = rotations.forall { RotationDirection(_) == rotationDirection }
    
    
    // COMPUTED PROPERTIES    ---------
    
    override def toShape = 
    {
        val polygon = new java.awt.Polygon()
        vertices.foreach { vertex => polygon.addPoint(vertex.x.toInt, vertex.y.toInt) }
        polygon
    }
    
    /**
     * The amount of vertices in this polygon
     */
    def size = vertices.size
    
    /**
     * The collision axes of this polygon. Only axes that are not paraller with each other are 
     * included.
     */
    def axes = if (size == 2) edge(0).collisionAxes else edges.map { _.vector.normal2D }.withDistinct { _ isParallelWith _ }
    
    /**
     * Divides this polygon into convex portions. Each of the returned parts is convex and can 
     * be used in collision checks
     */
    def convexParts: Vector[Polygon] = 
    {
        if (isConvex || size < 3)
        {
            Vector(this)
        }
        else
        {
            val firstBrokenIndex = rotations.indexWhere { RotationDirection(_) != rotationDirection }
            
            // Tries to find another (non-sequential) broken index
            val secondBrokenIndex = if (firstBrokenIndex < size - 1) 
                    rotations.indexWhere({ RotationDirection(_) != rotationDirection }, 
                    firstBrokenIndex + 1) else -1;
            
            if (secondBrokenIndex >= 0)
            {
                // If a second index was found, cuts the polygon between the two indices
                cutBetween(firstBrokenIndex, secondBrokenIndex).flatMap { _.convexParts }
            }
            else 
            {
                // If there is only one broken index, cuts the polygon so that the part becomes convex
                val brokenVertex = vertex(firstBrokenIndex)
                val incomeAngle = edge(firstBrokenIndex - 1).vector.direction
                
                val remainingOutcomeIndex = if (firstBrokenIndex < size - 2) vertices.indexWhere( 
                        vertex => { RotationDirection((vertex - brokenVertex).direction - 
                        incomeAngle) == rotationDirection }, firstBrokenIndex + 2) else -1;
                
                if (remainingOutcomeIndex >= 0)
                {
                    cutBetween(firstBrokenIndex, remainingOutcomeIndex).flatMap { _.convexParts }
                }
                else
                {
                    val outcomeIndex = vertices.indexWhere { vertex => RotationDirection(
                        (vertex - brokenVertex).direction - incomeAngle) == rotationDirection }
                    
                    cutBetween(outcomeIndex, firstBrokenIndex).flatMap { _.convexParts }
                }
            }
        }
    }
    
    /**
     * The middle-most point of the polygon
     */
    def center = Vector3D.average(vertices)
    
    /**
     * The smallest possible circle that contains all the vertices in the polygon
     */
    def circleAround = 
    {
        val origin = center
        val radius = vertices.map { vertex => (vertex - origin).length }.max
        Circle(origin, radius)
    }
    
    /**
     * The largest possible circle that fits inside the polygon
     */
    def circleInside = 
    {
        val origin = center
        val radius = vertices.map { vertex => (vertex - origin).length }.min
        Circle(origin, radius)
    }
    
    private def rotations = for { i <- 0 until size } yield rotation(i)
    
    
    // IMPLEMENTED METHODS    ---------
    
    /**
     * Checks whether the polygon contains the specified 2D point
     */
    override def contains(point: Vector3D): Boolean = 
    {
        if (vertices.contains(point))
        {
            // Has to check first if the checked point matches any of the vertices
            true
        }
        else if (size < 3)
        {
            // Polygons with less than 3 vertices cannot contain any points
            false
        }
        else if (isConvex)
        {
            // Contains only works for convex polygons
            // Finds the vertex that is closest to the target point
            val vertexInfo = for { i <- 0 until size } yield { (vertex(i), (vertex(i) - point).length, i) }
            val closestIndex = vertexInfo.minBy { _._2 }._3
            
            // Finds the edge that is closest to the target point
            val nextIndex = if (closestIndex < size - 1) closestIndex + 1 else 0
            val previousIndex = if (closestIndex > 0) closestIndex - 1 else size - 1
            
            val edge = if (vertexInfo(previousIndex)._2 < vertexInfo(nextIndex)._2) 
                    Line(vertex(previousIndex), vertex(closestIndex)) else 
                    Line(vertex(closestIndex), vertex(nextIndex));
            
            // The polygon contains the point if adding another edge though the target point would 
            // make the polygon non-convex
            RotationDirection((edge.end - point).direction - (point - edge.start).direction) != rotationDirection
        }
        else
        {
            // Non-convex polygons are divided into convex parts and then checked
            convexParts.exists { _.contains(point) }
        }
    }
    
    override def contains2D(point: Vector3D) = contains(point)
    
    override def transformedWith(transformation: Transformation) = Polygon(vertices.map(transformation.apply))
    
    override def projectedOver(axis: Vector3D) = 
    {
        if (vertices.isEmpty)
        {
            Line(Vector3D.zero, Vector3D.zero)
        }
        else
        {
            val projections = vertices.map { _ projectedOver axis }
            Line(projections.reduce(Vector3D.min), projections.reduce(Vector3D.max))
        }
    }
    
    
    // OTHER METHODS    ---------------
    
    /**
     * Finds a specific verse from this polygon. The indexing loops around so it's safe to use 
     * without fear of OutOfBoundsExceptions
     * @param index the index from which the vertex is retrieved (looped)
     */
    def vertex(index: Int) = 
    {
        // Makes sure the index is within range
        val vertexAmount = vertices.size
        
        if (index < 0)
        {
            var positiveIndex = index
            while (positiveIndex < 0) { positiveIndex += vertexAmount }
            vertices(positiveIndex)
        }
        else if (index >= vertexAmount)
        {
            vertices(index % vertexAmount)
        }
        else
        {
            vertices(index)
        }
    }
    
    /**
     * Finds a specific edge from this polygon. The first edge (0) is the one that starts from the 
     * first vertex of this polygon
     * @param the index of the edge / starting vertex (looping)
     */
    def edge(index: Int) = Line(vertex(index), vertex(index + 1))
    
    /**
     * Returns a copy of this polygon with the specified rotation direction
     */
    def withRotationDirection(direction: RotationDirection) = 
            if (rotationDirection == direction) this else Polygon(vertices.reverse);
    
    /**
     * Slices the polygon to two pieces. The cut is made between the two vertices so that both 
     * polygon pieces will contain those vertices.
     * @param index1 The index of the first common index (< index2 - 1)
     * @param index2 The index of the second common index (> index 1 + 1)
     * @return Two polygon pieces (vector of size 2)
     */
    def cutBetween(index1: Int, index2: Int) = 
    {
        val cutVertices = vertices.slice(index1, index2 + 1)
        val remainingVertices = vertices.take(index1 + 1) ++ vertices.drop(index2)
        Vector(Polygon(remainingVertices), Polygon(cutVertices))
    }
    
    /**
     * The rotation / angle between two edges connected to the specified vertex, in radians
     */
    private def rotation(index: Int) = edge(index).vector.direction - edge(index - 1).vector.direction
    
    /**
     * Checks if there's collision between the two polygon instances. Returns collision data if 
     * there is collision
     */
    def checkCollisionWith(other: Polygon) = 
    {
        if (isConvex && other.isConvex)
        {
            checkCollisionWithConvex(other)
        }
        else
        {
            val myParts = convexParts
            val otherParts = other.convexParts
            
            myParts.flatMap { myPart => otherParts.flatMap { 
                    myPart.checkCollisionWithConvex(_) } }.reduceOption { _ + _ }
        }
    }
    
    /**
     * Checks if there's collision between the two polygon instances. Returns collision data if 
     * there is collision. <b>Only works with convex polygons</b>
     */
    def checkCollisionWithConvex(other: Polygon) = 
    {
        // Uses collision axes from both polygons, doesn't need to repeat parallel axes
        val collisionAxes = (axes ++ other.axes).withDistinct { _ isParallelWith _ }
        val mtv = collisionMtvWith(other, collisionAxes)
        
        mtv.map { mtv => new Collision(mtv, collisionPoints(other, mtv)) }
    }
    
    /**
     * Checks if there's collision between this polygon and the provided circle shape. Returns 
     * collision data if there is a collision. The collision points are calculated by iterating 
     * through each of the polygon's edges, which is very taxing. If the accuracy of the collision 
     * points is less important, you should use <i>checkApproximateCollisionWith(Circle, Int)</i> instead
     */
    def checkCollisionWith(circle: Circle) = 
    {
        val mtv = collisionMtvWith(circle, axes :+ (center - circle.origin))
        mtv.map { new Collision(_, collisionPoints(circle).toVector) }
    }
    
    /**
     * Checks if there's collision between this polygon and the provided circle shape. Returns 
     * collision data if there is a collision. The collision points are approximated by transforming 
     * the circle into a polygon. They are not as accurate but are also less taxing to calculate. If 
     * the accuracy of the collision points is very important, you might want to use 
     * <i>checkCollisionWith(Circle)</i> instead.
     */
    def checkApproximateCollisionWith(circle: Circle, circleToPolygonEdges: Int = 12) = 
    {
        val mtv = collisionMtvWith(circle, axes :+ (center - circle.origin))
        mtv.map { mtv => new Collision(mtv, collisionPoints(circle.toPolygon(circleToPolygonEdges), mtv)) }
    }
    
    /**
     * Checks if there's collision between this polygon and a line segment. Returns collision data 
     * if there is a collision
     */
    def checkCollisionWith(line: Line) = 
    {
        val mtv = collisionMtvWith(line, axes ++ line.collisionAxes)
        mtv.map { mtv => new Collision(mtv, collisionPoints(line, mtv)) }
    }
    
    /**
     * Finds the collision points between two (colliding) <b>convex</b> polygons
     * @param other The other polygon
     * @param collisionNormal A normal for the collision plane, usually the minimum translation 
     * vector for this polygon 
     */
    def collisionPoints(other: Polygon, collisionNormal: Vector3D) = 
    {
        if (size < 2 || other.size < 2)
        {
            // Collision checks don't work with < 2 vertex polygons
            Vector()
        }
        else 
        {
            edgeCollisionClip(other.collisionEdge(-collisionNormal), collisionNormal)
        }
    }
    
    /**
     * Finds the collision points between this polygon and a circle. NB: This operation is somewhat 
     * slow (O(n)) and should be used sparingly and only when a collision has already been recognised.
     */
    def collisionPoints(circle: Circle) = edges.flatMap { _.circleIntersection(circle) }
    
    /**
     * Finds the collision points between this polygon and a line when collision normal (mtv) is 
     * already known
     * @param line any line
     * @param collisionNormal a normal to the collision, pointing from the collision area towards 
     * this polygon (ie. The collision mtv for this polygon)
     */
    def collisionPoints(line: Line, collisionNormal: Vector3D) = 
    {
        if (size < 2)
        {
            Vector()
        }
        else
        {
            // The collision edge always starts at the point closer to the collision area 
            // (= more perpendicular to the collision normal)
            val otherEdge = if (math.abs(line.start dot collisionNormal) < 
                    math.abs(line.end dot collisionNormal)) line else line.reverse
            edgeCollisionClip(otherEdge, collisionNormal)
        }
    }
    
    /**
     * Clips the collision points from two collision edges when the collision normal (mtv) is known
     * @param otherCollisionEdge the collision edge of the other shape in the collision
     * @param collisionNormal a normal for the collision plane, from the collision area towards 
     * this polygon instance (ie. the mtv for this polygon)
     */
    private def edgeCollisionClip(otherCollisionEdge: Line, collisionNormal: Vector3D) =
    {
        // Finds the remaining (own) collision edge
        val myCollisionEdge = collisionEdge(collisionNormal)
        
        // The reference edge is the one that is more perpendicular to the collision normal
        if (math.abs(myCollisionEdge.vector dot collisionNormal) <= 
                math.abs(otherCollisionEdge.vector dot collisionNormal))
        {
            Polygon.clipCollisionPoints(myCollisionEdge, otherCollisionEdge, collisionNormal)
        }
        else
        {
            Polygon.clipCollisionPoints(otherCollisionEdge, myCollisionEdge, -collisionNormal)
        }
    }
    
    // Use minimum translation vector as normal (points towards this polygon from the collision area)
    // Doesn't work for polygons with < 2 vertices (surprise)
    private def collisionEdge(collisionNormal: Vector3D) = 
    {
        // Finds the vertex closest to the collision direction
        val closestVertexIndex = (for { i <- 0 until size } yield 
                (i, vertex(i) dot collisionNormal)).minBy { _._2 }._1
                
        // Uses the edge that is more perpendicular to the collision normal
        Vector(Line(vertex(closestVertexIndex), vertex(closestVertexIndex - 1)), 
                Line(vertex(closestVertexIndex), vertex(closestVertexIndex + 1))
                ).minBy { _.vector dot collisionNormal }
    }
}