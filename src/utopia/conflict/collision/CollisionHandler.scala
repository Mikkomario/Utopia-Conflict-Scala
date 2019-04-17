package utopia.conflict.collision

import utopia.conflict.handling.CollisionListener
import utopia.inception.handling.HandlerType
import utopia.inception.handling.Handler
import utopia.genesis.event.Actor

case object CollisionHandlerType extends HandlerType(classOf[CollisionListener])

/**
 * A collision handler handles collision checking between collisionListeners and collidable 
 * instances. A collisionHandler needs to be added to a working ActorHandler in order to function 
 * properly.
 * @author Mikko Hilpinen
 * @since 4.8.2017
 */
class CollisionHandler(val collidableHandler: CollidableHandler) extends 
        Handler[CollisionListener](CollisionHandlerType) with Actor
{
    // IMPLEMENTED METHODS    ------------------
    
    override def act(durationMillis: Double) = 
    {
        // Checks collisions for each of the listeners
        foreach(true, listener => 
        {
            // Doesn't include collisions with the listener itself, naturally
            val collisions = collidableHandler.checkForCollisions(listener.collisionShape, 
                    listener.targetCollisionGroups).filterNot { _._1 == listener }
            
            if (!collisions.isEmpty)
            {
                listener.onCollision(collisions, durationMillis)
            }
            
            true
        })
    }
}