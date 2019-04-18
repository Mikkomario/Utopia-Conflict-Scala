package utopia.conflict.test

import utopia.genesis.util.Vector3D
import utopia.genesis.view.Canvas
import utopia.genesis.view.MainFrame
import utopia.genesis.event.ActorThread
import utopia.genesis.view.CanvasMouseEventGenerator
import utopia.inception.handling.HandlerRelay
import utopia.conflict.util.Polygon
import utopia.genesis.util.Transformation
import utopia.conflict.collision.Extensions._
import utopia.genesis.util.Circle
import utopia.conflict.collision.CollidableHandler
import utopia.conflict.collision.CollisionHandler
import utopia.genesis.util.Bounds
import utopia.conflict.test.TestCollisionGroups.Obstacle
import scala.collection.immutable.HashSet

/**
 * This test visually displays collision data with interactive elements
 * @author Mikko Hilpinen
 * @since 4.8.2017
 */
object CollisionTest extends App
{
    val worldSize = Vector3D(800, 600)
    
    val canvas = new Canvas(worldSize, 120)
    val frame = new MainFrame(canvas, worldSize, "Collision Test")
    
    val actorThread = new ActorThread(20, 120)
    val mouseEventGen = new CanvasMouseEventGenerator(canvas)
    actorThread.handler += mouseEventGen
    
    val collidableHandler = new CollidableHandler()
    val collisionHandler = new CollisionHandler(collidableHandler)
    actorThread.handler += collisionHandler
    
    val handlers = new HandlerRelay(canvas.handler, actorThread.handler, mouseEventGen.moveHandler, 
            mouseEventGen.buttonStateHandler, collidableHandler, collisionHandler)
    
    val simplePolygon = Polygon(Vector(Vector3D(-32, -32), Vector3D(0, 64), Vector3D(32, 32), Vector3D.zero))
    val transformedPolygon = Transformation.translation(worldSize / 2).scaled(2)(simplePolygon)
    
    val nonConvexPolygon = Polygon(Vector(Vector3D(-32, -32), Vector3D(-0.5), Vector3D(-32, 32), Vector3D(32, 32), Vector3D(0.5), Vector3D(32, -32)))
    
    val obstacle1 = new TestPolygonObstacle(transformedPolygon)
    val obstacle2 = new TestPolygonObstacle(Circle(Vector3D(96, 228), 64).toPolygon(12))
    val obstacle3 = new TestPolygonObstacle(Bounds(worldSize - Vector3D(128, 128), Vector3D(64, 64)))
    val obstacle4 = new TestPolygonObstacle(Transformation.translation(Vector3D(worldSize.x - 128, 32))(nonConvexPolygon))
    
    val obstacle5 = new TestCircleObstacle(Circle(worldSize - Vector3D(128, 128), 96))
    
    // val mouseObstacle = new MousePolygonObstacle(Polygon(Vector(Vector3D(24), Vector3D(0, -24), Vector3D(-24), Vector3D(0, 24))))
    val mouseObstacle = new MousePolygonObstacle(Bounds(Vector3D(-32, -32), Vector3D(64, 64)))
    
    val collisionDrawer = new CollisionDrawer(mouseObstacle, Some(HashSet(Obstacle)))
    val projectionDrawer = new ProjectionDrawer(transformedPolygon)
    
    handlers += obstacle1
    handlers += obstacle2
    handlers += obstacle3
    handlers += obstacle4
    handlers += obstacle5
    handlers += mouseObstacle
    handlers += collisionDrawer
    handlers += projectionDrawer
    
    /*
    val grid = new GridDrawer(worldSize, Vector3D(80, 80))
    val numbers = new GridNumberDrawer(grid)
    val camera = new MagnifierCamera(64)
    
    handlers += grid
    handlers += numbers
    handlers += camera
    handlers += camera.drawHandler
    
    camera.drawHandler += grid
    camera.drawHandler += numbers
    */
    
    actorThread.start()
    frame.display()
}