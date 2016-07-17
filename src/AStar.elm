module AStar
    exposing
        ( Position
        , Path
        , findPath
        , horizontalCost
        )

{-| The A* pathfinding algorithm.

@docs Position
@docs Path
@docs findPath
@docs horizontalCost
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


{-| A position is just a pair of (x,y) coordinates.
-}
type alias Position =
    ( Int, Int )


{-| A path is an `Array` of `Position`s.
-}
type alias Path =
    Array Position


{-| Find a path between the `start` and `end` `Position`s. You must
  supply a cost function and a move function.

  The cost function must estimate the distance between any two
  positions. It doesn't really matter how accurate this estimate is,
  as long as it *never* underestimates.

  The move function takes a `Position` and returns a `Set` of possible
  places you can move to in one step.

  If this function returns `Nothing`, there is no path between the two
  points. Otherwise it returns `Just` an `Array` of steps from `start`
  to `end`.

  Example usage.

     import AStar exposing (..)


     findPath horizontalCost movesFrom ( 0, 0 ) ( 2, 0 )
     --> Just (Array.fromList [ ( 1, 0 ), ( 2, 0 ) ])


-}
findPath :
    (Position -> Position -> Float)
    -> (Position -> Set Position)
    -> Position
    -> Position
    -> Maybe Path
findPath costFn moveFn start end =
    initialModel start
        |> astar costFn moveFn end


{-| A simple costing algorithm. Think of it as the number of moves a
rook/castle would have to make on a chessboard. Even if your piece can
move diagonally it will still work! A* only requires that the cost
function never *under*estimates.
-}
horizontalCost : Position -> Position -> Float
horizontalCost ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            abs (x1 - x2)

        dy =
            abs (y1 - y2)
    in
        toFloat <| dx + dy


type alias Model =
    { evaluated : Set Position
    , openSet : Set Position
    , costs : Dict Position Float
    , cameFrom : Dict Position Position
    }


initialModel : Position -> Model
initialModel start =
    { evaluated = Set.empty
    , openSet = Set.singleton start
    , costs = Dict.singleton start 0
    , cameFrom = Dict.empty
    }


cheapestOpen : (Position -> Float) -> Model -> Maybe Position
cheapestOpen costFn model =
    model.openSet
        |> Set.toList
        |> List.filterMap
            (\position ->
                case Dict.get position model.costs of
                    Nothing ->
                        Nothing

                    Just cost ->
                        Just ( position, cost + costFn position )
            )
        |> List.sortBy snd
        |> List.head
        |> Maybe.map fst


reconstructPath : Dict Position Position -> Position -> Path
reconstructPath cameFrom goal =
    case Dict.get goal cameFrom of
        Nothing ->
            Array.empty

        Just next ->
            Array.push goal
                (reconstructPath cameFrom next)


updateCost : Position -> Position -> Model -> Model
updateCost current neighbour model =
    let
        newCameFrom =
            Dict.insert neighbour current model.cameFrom

        distanceTo =
            reconstructPath newCameFrom neighbour
                |> Array.length
                |> toFloat

        newModel =
            { model
                | costs = Dict.insert neighbour distanceTo model.costs
                , cameFrom = newCameFrom
            }
    in
        case Dict.get neighbour model.costs of
            Nothing ->
                newModel

            Just previousDistance ->
                if distanceTo < previousDistance then
                    newModel
                else
                    model


astar : (Position -> Position -> Float) -> (Position -> Set Position) -> Position -> Model -> Maybe Path
astar costFn moveFn goal model =
    case cheapestOpen (costFn goal) model of
        Nothing ->
            Nothing

        Just current ->
            if current == goal then
                Just (reconstructPath model.cameFrom goal)
            else
                let
                    modelPopped =
                        { model
                            | openSet = Set.remove current model.openSet
                            , evaluated = Set.insert current model.evaluated
                        }

                    neighbours =
                        moveFn current

                    newNeighbours =
                        Set.diff neighbours modelPopped.evaluated

                    modelWithNeighbours =
                        { modelPopped
                            | openSet =
                                Set.union modelPopped.openSet
                                    newNeighbours
                        }

                    modelWithCosts =
                        Set.foldl (updateCost current) modelWithNeighbours newNeighbours
                in
                    astar costFn moveFn goal modelWithCosts
