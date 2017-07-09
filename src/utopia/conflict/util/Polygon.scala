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

/**
 * Polygons are used for representing more complicated shapes. Polygons only support 2D shapes on 
 * the x-y -plane
 * @author Mikko Hilpinen
 * @since 25.6.2017
 */
case class Polygon(val vertices: Vector[Vector3D]) extends Area with ShapeConvertible with 
        TransformableShape[Polygon]
{
    // ATTRIBUTES    ------------------
    
    /**
     * The top left corner of a bounding box around this polygon
     */
    lazy val min = if (vertices.isEmpty) Vector3D.zero else vertices.reduce(Vector3D.min)
    
    /**
     * The bottom right corner of a bounding box around this polygon
     */
    lazy val max = if (vertices.isEmpty) Vector3D.zero else vertices.reduce(Vector3D.max)
    
    /**
     * A bounding box around this polygon. All of the polygon's points lay inside its bounds
     */
    lazy val bounds = Bounds.between(min, max)
    
    /**
     * The edges that form the sides of this polygon
     */
    lazy val edges = for { i <- 0 until size } yield edge(i)
    
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
    def axes = edges.map { _.vector.normal2D }.withDistinct { _ isParallelWith _ }
    
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
            val closestIndex = vertexInfo.sortBy { _._2 }.head._3
            
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
}