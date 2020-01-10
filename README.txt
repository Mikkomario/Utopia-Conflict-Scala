UTOPIA CONFLICT
---------------

Required Libraries
------------------
    - Utopia Flow
    - Utopia Inception
    - Utopia Genesis


Purpose
-------

    Utopia Conflict handles 2D collision detection and collision events, which are a complex feature often used in
    various real-time games.


Main Features
-------------

    2D collision handling with Collidable, CollisionListener, CollidableHandler and CollisionHandler traits
        - Follows the Handleable + Handler design logic from Genesis and Inception
        - Both mutable and immutable implementations available
        - Supports advanced polygonic shapes from Genesis, as well as circles
        - Collision events provide access to collision (intersection) points as well as a minimum translation
        vector (MTV) which helps the receiver to resolve the collision situation (with translation, for example)


Usage Notes
-----------

    CollisionHandler needs to be added to an ActorHandler in order to work


Available Extensions
--------------------

    utopia.conflict.collision.Extensions
        - Adds collision handling to Circle and Polygonic
        - Adds conversion from Circles to Polygons


v 1.1 (beta)    --------------

    Updates & Changes
    -----------------

        CollisionListeners now take FiniteDuration as a duration parameter instead of java.time.Duration


v1  --------------------------

    New Features

        Collidable & CollisionListener traits

        CollidableHandler & CollisionHandler
            + Both mutable and immutable implementations

    Updates & Changes

        Package structure updated
            util -> collision
            collision -> handling

        Polygon class moved to Genesis. Only Polygonic instances are handled now.
