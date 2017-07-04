package utopia.conflict.util

import utopia.genesis.util.Vector3D
import utopia.genesis.util.Bounds
import utopia.genesis.util.Line
import scala.collection.mutable.ListBuffer
import utopia.genesis.util.Extensions._

/**
 * Polygons are used for representing more complicated shapes. Polygons only support 2D shapes on 
 * the x-y -plane
 * @author Mikko Hilpinen
 * @since 25.6.2017
 */
case class Polygon(val vertices: Vector[Vector3D])
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
    
    
    // COMPUTED PROPERTIES    ---------
    
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
     * Checks whether the polygon is convex. Convex polygons only need to turn clockwise or 
     * counter-clockwise when traversing through the polygon. They don't have spikes or holes, 
     * so to speak.
     */
    def isConvex = rotations.forall { RotationDirection(_) != rotationDirection }
    
    def convexParts: Vector[Polygon] = 
    {
        // Finds the first non-convex index
        val firstBrokenIndex = rotations.indexWhere { RotationDirection(_) != rotationDirection }
        
        if (firstBrokenIndex >= 0 && size > 2)
        {
            // Tries to find another (non-sequential) broken index
            val secondBrokenIndex = if (firstBrokenIndex < size - 1) 
                    rotations.indexWhere({ RotationDirection(_) != rotationDirection }, 
                    firstBrokenIndex + 1) else -1;
            
            if (secondBrokenIndex >= 0)
            {
                // If a second index was found, cuts the polygon between the two indices
                val cutVertices = vertices.slice(firstBrokenIndex, secondBrokenIndex + 1)
                val remainingVertices = vertices.take(firstBrokenIndex + 1) ++ vertices.drop(
                        secondBrokenIndex);
                
                Polygon(cutVertices).convexParts ++ Polygon(remainingVertices).convexParts
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
                    val cutVertices = vertices.slice(firstBrokenIndex, remainingOutcomeIndex + 1)
                    val remainingVertices = vertices.take(firstBrokenIndex + 1) ++ vertices.drop(
                            remainingOutcomeIndex);
                    
                    Polygon(cutVertices).convexParts ++ Polygon(remainingVertices).convexParts
                }
                else
                {
                    val outcomeIndex = vertices.indexWhere { vertex => RotationDirection(
                        (vertex - brokenVertex).direction - incomeAngle) == rotationDirection }
                    
                    val cutVertices = vertices.drop(firstBrokenIndex) ++ vertices.take(outcomeIndex + 1)
                    val remainingVertices = vertices.slice(outcomeIndex, firstBrokenIndex + 1)
                    
                    Polygon(cutVertices).convexParts ++ Polygon(remainingVertices).convexParts
                }
            }
        }
        else 
        {
            Vector(this)
        }
    }
    
    private def rotations = for { i <- 0 until size } yield rotation(i)
    
    
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
     * The rotation / angle between two edges connected to the specified vertex, in radians
     */
    private def rotation(index: Int) = edge(index + 1).vector.direction - edge(index).vector.direction
}