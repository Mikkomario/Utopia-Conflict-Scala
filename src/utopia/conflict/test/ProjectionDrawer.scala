package utopia.conflict.test

import utopia.genesis.util.Projectable
import utopia.genesis.util.Vector3D
import utopia.genesis.util.Line
import utopia.genesis.event.Drawable
import utopia.genesis.util.Drawer
import java.awt.Color
import utopia.genesis.event.MouseButtonStateHandlerType
import utopia.genesis.event.MouseButtonStateListener
import utopia.genesis.event.MouseButtonStateEvent
import utopia.genesis.event.MouseButton
import utopia.genesis.event.MouseMoveListener
import utopia.genesis.event.MouseMoveEvent
import utopia.genesis.util.Transformation

/**
 * This object visually displays shape projection on a line drawn by the user
 * @author Mikko Hilpinen
 * @since 5.8.2017
 */
class ProjectionDrawer(val target: Projectable) extends Drawable with MouseButtonStateListener 
        with MouseMoveListener
{
    // ATTRIBUTES    ---------------------
    
    private var lastClickPosition = Vector3D.zero
    private var mouseLine = Line(Vector3D.zero, Vector3D.zero)
    private var projection = Line(Vector3D.zero, Vector3D.zero)
    
    
    // IMPLEMENTED PROPERTIES    ---------
    
    override val mouseButtonStateEventFilter = MouseButtonStateEvent.buttonFilter(MouseButton.Left)
    
    
    // INITIAL CODE    -------------------
    
    // Mouse clicks are always listened while other events are ignored while the mouse is not down
    specifyHandlingState(MouseButtonStateHandlerType)
    defaultHandlingState = false
    
    
    // IMPLEMENTED METHODS    ------------
    
    override def draw(drawer: Drawer) = 
    {
        drawer.withEdgeColor(Some(Color.GRAY)).draw(mouseLine)
        drawer.withEdgeColor(Some(Color.RED)).draw(projection)
    }
    
    override def onMouseButtonState(event: MouseButtonStateEvent) = 
    {
        if (event.isDown)
        {
            // On mouse press, starts listening to other events again, resets drawn lines
            lastClickPosition = event.mousePosition
            mouseLine = Line(Vector3D.zero, Vector3D.zero)
            projection = Line(Vector3D.zero, Vector3D.zero)
            
            defaultHandlingState = true
        }
        else
        {
            defaultHandlingState = false
        }
    }
    
    override def onMouseMove(event: MouseMoveEvent) = 
    {
        // Creates a new projection
        mouseLine = Line(lastClickPosition, event.mousePosition)
        projection = Transformation.translation(lastClickPosition)(target.projectedOver(mouseLine.vector))
    }
}