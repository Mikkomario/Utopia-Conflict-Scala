package utopia.conflict.test

import utopia.conflict.handling.mutable.{CollidableHandler, CollisionHandler}
import utopia.genesis.view.Canvas
import utopia.genesis.view.MainFrame
import utopia.genesis.view.CanvasMouseEventGenerator
import utopia.conflict.test.TestCollisionGroups.Obstacle
import utopia.genesis.handling.ActorLoop
import utopia.genesis.handling.mutable.{ActorHandler, DrawableHandler, MouseButtonStateHandler, MouseMoveHandler, MouseWheelHandler}
import utopia.genesis.shape.shape2D.{Bounds, Circle, Point, Polygon, Size, Transformation}
import utopia.inception.handling.mutable.HandlerRelay
import utopia.conflict.collision.Extensions._
import utopia.flow.async.ThreadPool
import utopia.genesis.shape.Vector3D
import utopia.genesis.util.FPS

import scala.collection.immutable.HashSet
import scala.concurrent.ExecutionContext

/**
 * This test visually displays collision data with interactive elements
 * @author Mikko Hilpinen
 * @since 4.8.2017
 */
object CollisionTest extends App
{
    // Creates the handlers
    val drawHandler = DrawableHandler()
    val actorHandler = ActorHandler()
    val mouseStateHandler = MouseButtonStateHandler()
    val mouseMoveHandler = MouseMoveHandler()
    val mouseWheelHandler = MouseWheelHandler()
    
    val collidableHandler = CollidableHandler()
    val collisionHandler = CollisionHandler(collidableHandler)
    actorHandler += collisionHandler
    
    // Creates the view
    val worldSize = Size(800, 600)
    
    val canvas = new Canvas(drawHandler, worldSize)
    val frame = new MainFrame(canvas, worldSize, "Collision Test")
    
    // Creates event generators
    val actorLoop = new ActorLoop(actorHandler, 20 to 120)
    val mouseEventGen = new CanvasMouseEventGenerator(canvas, mouseMoveHandler, mouseStateHandler, mouseWheelHandler)
    actorHandler += mouseEventGen
    
    // Creates handler relay and objects
    val handlers = HandlerRelay(drawHandler, actorHandler, mouseStateHandler, mouseMoveHandler, mouseWheelHandler,
            collidableHandler, collisionHandler)
    
    val simplePolygon = Polygon(Point(-32, -32), Point(0, 64), Point(32, 32), Point.origin)
    val transformedPolygon = Transformation.translation(worldSize.toVector / 2).scaled(2)(simplePolygon)
    
    val nonConvexPolygon = Polygon(Point(-32, -32), Point(-0.5, 0), Point(-32, 32), Point(32, 32), Point(0.5, 0), Point(32, -32))
    
    val obstacle1 = new TestPolygonObstacle(transformedPolygon)
    val obstacle2 = new TestPolygonObstacle(Circle(Point(96, 228), 64).toPolygon(12))
    val obstacle3 = new TestPolygonObstacle(Bounds(worldSize.toPoint - (128, 128), Size(64, 64)))
    val obstacle4 = new TestPolygonObstacle(Transformation.translation(Vector3D(worldSize.x - 128, 32))(nonConvexPolygon))
    
    val obstacle5 = new TestCircleObstacle(Circle(worldSize.toPoint - (128, 128), 96))
    
    // val mouseObstacle = new MousePolygonObstacle(Polygon(Vector(Vector3D(24), Vector3D(0, -24), Vector3D(-24), Vector3D(0, 24))))
    val mouseObstacle = new MousePolygonObstacle(Bounds(Point(-32, -32), Size(64, 64)))
    
    val collisionDrawer = new CollisionDrawer(mouseObstacle, Some(HashSet(Obstacle)))
    val projectionDrawer = new ProjectionDrawer(transformedPolygon)
    
    handlers ++= (obstacle1, obstacle2, obstacle3, obstacle4, obstacle5, mouseObstacle, collisionDrawer, projectionDrawer)
    
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
    
    // Starts the program
    implicit val context: ExecutionContext = new ThreadPool("Test").executionContext
    
    actorLoop.startAsync()
    canvas.startAutoRefresh(FPS(120))
    frame.display()
}