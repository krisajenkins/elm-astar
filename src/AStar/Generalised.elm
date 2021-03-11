module AStar.Generalised exposing (findPath)

{-| This is the real algorithm. The types are generalised here.

We publish simple types to the user which assume they're working with
a 2D grid.

But in reality, we aren't tied to (x,y) positions and we don't care
what the terrain model is. Positions can be any type you like (as long
as it's `comparable` so that we can use it as a dictionary
key).

But...that makes the type signatures pretty unintuitive to newer
users, so we hide them away here.

@docs findPath

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Tuple exposing (first, second)


{-| Find a path between the `start` and `end` `Position`s. You must
supply a neighbour cost function, a heuristic function and a move function.

See `AStar.findPath` for a getting-started guide. This is a more
general version of that same function.

-}
findPath :
    (comparable -> comparable -> Float)
    -> (comparable -> comparable -> Float)
    -> (comparable -> Set comparable)
    -> comparable
    -> comparable
    -> Maybe (List comparable)
findPath neighbourCostFn heuristicFn moveFn start end =
    initialModel start
        |> astar neighbourCostFn heuristicFn moveFn end
        |> Maybe.map Array.toList


type alias Model comparable =
    { evaluated : Set comparable
    , openSet : Set comparable
    , costs : Dict comparable Float
    , cameFrom : Dict comparable comparable
    }


initialModel : comparable -> Model comparable
initialModel start =
    { evaluated = Set.empty
    , openSet = Set.singleton start
    , costs = Dict.singleton start 0
    , cameFrom = Dict.empty
    }


cheapestOpen : (comparable -> Float) -> Model comparable -> Maybe comparable
cheapestOpen heuristicFn model =
    model.openSet
        |> Set.toList
        |> List.filterMap
            (\position ->
                case Dict.get position model.costs of
                    Nothing ->
                        Nothing

                    Just cost ->
                        Just ( position, cost + heuristicFn position )
            )
        |> List.sortBy second
        |> List.head
        |> Maybe.map first


reconstructPath : Dict comparable comparable -> comparable -> Array comparable
reconstructPath cameFrom goal =
    case Dict.get goal cameFrom of
        Nothing ->
            Array.empty

        Just next ->
            Array.push goal
                (reconstructPath cameFrom next)


updateCost : (comparable -> comparable -> Float) -> comparable -> comparable -> Model comparable -> Model comparable
updateCost neighbourCostFn current neighbour model =
    let
        newCameFrom =
            Dict.insert neighbour current model.cameFrom

        newCosts =
            Dict.insert neighbour distanceTo model.costs

        distanceTo =
            neighbourCostFn current neighbour
                + (reconstructPath newCameFrom neighbour
                    |> Array.length
                    |> toFloat
                  )

        newModel =
            { model
                | costs = newCosts
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


astar :
    (comparable -> comparable -> Float)
    -> (comparable -> comparable -> Float)
    -> (comparable -> Set comparable)
    -> comparable
    -> Model comparable
    -> Maybe (Array comparable)
astar neighbourCostFn heuristicFn moveFn goal model =
    case cheapestOpen (heuristicFn goal) model of
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
                        Set.foldl (updateCost neighbourCostFn current) modelWithNeighbours newNeighbours
                in
                astar neighbourCostFn heuristicFn moveFn goal modelWithCosts
