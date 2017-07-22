package utopia.conflict.util

import utopia.genesis.util.Bounds
import scala.language.implicitConversions
import utopia.genesis.util.Circle
import utopia.genesis.util.Vector3D

/**
 * This object contains extensions that are used in the conflict project
 * @author Mikko Hilpinen
 * @since 13.7.2017
 */
object Extensions
{
    // Bounds can be represented as polygons where necessary
    implicit def boundsToPolygon(bounds: Bounds) = Polygon(bounds.corners2D)
}