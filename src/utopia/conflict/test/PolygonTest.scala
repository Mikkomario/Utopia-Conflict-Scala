package utopia.conflict.test

import utopia.genesis.generic.GenesisDataType
import utopia.conflict.util.Polygon
import utopia.genesis.util.Vector3D
import utopia.genesis.util.Line
import utopia.conflict.util.RotationDirection.Clockwise
import utopia.conflict.util.RotationDirection.CounterClockwise
import utopia.conflict.util.Extensions._
import utopia.genesis.util.Bounds

/**
 * This test tests the basic polygon features
 * @author Mikko Hilpinen
 * @since 6.7.2017
 */
object PolygonTest extends App
{
    GenesisDataType.setup()
    
    // Square
    val polygon = Polygon(Vector(Vector3D.zero, Vector3D(3), Vector3D(3, 3), Vector3D(0, 3)))
    
    // Tests basic vertex and edge accessing
    assert(polygon.vertex(1) == Vector3D(3))
    assert(polygon.vertex(4) == polygon.vertex(0))
    assert(polygon.vertex(-1) == polygon.vertex(3))
    assert(polygon.edge(0) == Line(Vector3D.zero, Vector3D(3)))
    
    // Tests other computed properties
    assert(polygon.topLeft == Vector3D.zero)
    assert(polygon.bottomRight == Vector3D(3, 3))
    
    assert(polygon.rotationDirection == Clockwise)
    assert(polygon.isConvex)
    assert(polygon.axes.size == 2)
    assert(polygon.axes.exists { _ isParallelWith Vector3D(1) })
    assert(polygon.convexParts.contains(polygon))
    
    assert(polygon.center == Vector3D(1.5, 1.5))
    assert(polygon.circleAround == polygon.circleInside)
    assert(polygon.circleAround.contains(Vector3D.zero))
    
    // Tests polygon splitting
    val parts = polygon.cutBetween(0, 2)
    
    assert(parts.size == 2)
    assert(parts.forall { _.size == 3 })
    assert(parts(0) != parts(1))
    
    // Tests containment
    assert(polygon.contains(Vector3D(0.3, 0.1)))
    assert(polygon.contains(Vector3D(1.5, 1.5)))
    assert(polygon.contains(Vector3D.zero))
    
    // Sand glass
    val polygon2 = Polygon(Vector(Vector3D.zero, Vector3D(0.5, 1), Vector3D(0, 2), Vector3D(2, 2), 
            Vector3D(1.5, 1), Vector3D(2, 0)));
    
    assert(polygon2.rotationDirection == CounterClockwise)
    assert(!polygon2.isConvex)
    
    val parts2 = polygon2.convexParts // polygon2.cutBetween(1, 4)
    
    assert(parts2.size == 2)
    assert(parts2.forall { _.isConvex })
    assert(parts2.forall { _.size == 4 })
    
    assert(polygon.projectedOver(Vector3D(1)) == Line(Vector3D.zero, Vector3D(3)))
    assert(polygon.projectedOver(Vector3D(0, 1)) == Line(Vector3D.zero, Vector3D(0, 3)))
    
    // Tests collision recognition
    val outsideBox = Bounds(Vector3D(0, -2), Vector3D(1, 1))
    
    assert(polygon.checkCollisionWith(outsideBox) == None)
    
    val overlappingBox = Bounds(Vector3D(1, -1), Vector3D(1, 2))
    val collision1 = overlappingBox.checkCollisionWith(polygon)
    
    assert(collision1.isDefined)
    assert(collision1.get.mtv == Vector3D(0, -1))
    
    println(collision1.get.collisionPoints)
    
    val boxInside = Bounds(Vector3D(1.5, 1), Vector3D(1, 1))
    val collision2 = boxInside.checkCollisionWith(polygon)
    
    assert(collision2.isDefined)
    assert(collision2.get.mtv == Vector3D(1.5))
    
    println(collision2.get.collisionPoints)
    
    println("Success!")
}