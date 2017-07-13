package utopia.conflict.util

/**
 * This object contains extensions that are used in the conflict project
 * @author Mikko Hilpinen
 * @since 13.7.2017
 */
object Extensions
{
    implicit class FailableIterable[T](val c: Iterable[T]) extends AnyVal
    {
        /**
         * This function maps values like a normal map function, but terminates immediately if 
         * None is returned by the transform function
         * @return The mapped collection or none if mapping failed for any element
         */
        def mapOrFail[B](f: T => Option[B]): Option[Vector[B]] = 
        {
            val iterator = c.iterator
            val buffer = Vector.newBuilder[B]
            
            while (iterator.hasNext)
            {
                val result = f(iterator.next())
                if (result.isDefined)
                {
                    buffer += result.get
                }
                else
                {
                    return None
                }
            }
            
            Some(buffer.result())
        }
    }
}