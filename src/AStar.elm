module AStar exposing
    ( Position
    , Path
    , findPath
    , constantNeighbourCost
    , straightLineHeuristic
    , pythagoreanHeuristic
    )

{-| The A-Star pathfinding algorithm.

@docs Position
@docs Path
@docs findPath
@docs constantNeighbourCost
@docs straightLineHeuristic
@docs pythagoreanHeuristic

-}

import AStar.Generalised
import Set exposing (Set)


{-| A position is just a pair of (x,y) coordinates.
-}
type alias Position =
    ( Int, Int )


{-| A path is a `List` of `Position`s.
-}
type alias Path =
    List Position


{-| Find a path between the `start` and `end` `Position`s. You must
supply a cost function and a move function.

The neighbour cost function allows you to introduce eg. different
terrains: the cost of moving to a highway might be 1, while the cost
of moving to the mountains might be 3. The algorithm will then take
that into account and find an optimal path (circling around the
mountain might be faster than trekking it in a straight line).

The heuristic function must estimate the distance between any two
positions. It doesn't really matter how accurate this estimate is,
as long as it _never_ overestimates.

The move function takes a `Position` and returns a `Set` of possible
places you can move to in one step.

If this function returns `Nothing`, there is no path between the two
points. Otherwise it returns `Just` a `List` of steps from `start`
to `end`.

Example usage.

     import AStar exposing (..)


     type World =
         ...your custom code...


     movesFrom : World -> Position -> Set Position
     movesFrom world (x,y) =
         ...your custom code...


     neighbourCost : Position -> Position -> Float
     neighbourCost from to =
         -- opting out of the terrain cost calculation
         0


     findPath
         neighbourCost
         straightLineHeuristic
         (movesFrom currentWorld)
         ( 0, 0 ) ( 2, 0 )
     --> Just [ ( 1, 0 ), ( 2, 0 ) ]

-}
findPath :
    (Position -> Position -> Float)
    -> (Position -> Position -> Float)
    -> (Position -> Set Position)
    -> Position
    -> Position
    -> Maybe Path
findPath =
    AStar.Generalised.findPath


{-| A neighbour cost function that always returns 0. Handy when all
neighbouring tiles have the same movement cost between them.
-}
constantNeighbourCost : Position -> Position -> Float
constantNeighbourCost _ _ =
    0


{-| A simple heuristic algorithm. Think of it as the number of moves a
rook/castle would have to make on a chessboard.
-}
straightLineHeuristic : Position -> Position -> Float
straightLineHeuristic ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            abs (x1 - x2)

        dy =
            abs (y1 - y2)
    in
    toFloat <| dx + dy


{-| An alternative heuristic algorithm, which calculates pythagorean distance.
-}
pythagoreanHeuristic : Position -> Position -> Float
pythagoreanHeuristic ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            toFloat <| abs (x1 - x2)

        dy =
            toFloat <| abs (y1 - y2)
    in
    abs <| (sqrt 2 * min dx dy) + abs (dy - dx)
