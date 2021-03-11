module AStarTests exposing (suite)

import AStar exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    describe "AStar"
        [ findPathTests
        , findPathFuzzTests
        , straightLineHeuristicFuzzTests
        ]


findPathTests : Test
findPathTests =
    describe "findPath"
        [ test "it finds a path" <|
            \() ->
                findPath
                    constantNeighbourCost
                    straightLineHeuristic
                    movesFrom
                    ( 0, 0 )
                    ( 2, 0 )
                    |> Expect.equal (Just [ ( 1, 0 ), ( 2, 0 ) ])
        , test "it finds the optimal path given non-uniform terrain" <|
            \() ->
                findPath
                    mountainMapNeighbourCost
                    straightLineHeuristic
                    movesFrom
                    ( 2, 0 )
                    ( 2, 2 )
                    |> Expect.equal
                        (Just
                            [ ( 1, 0 )
                            , ( 0, 1 )
                            , ( 1, 2 )
                            , ( 2, 2 )
                            ]
                        )
        ]


findPathFuzzTests : Test
findPathFuzzTests =
    describe "findPathFuzzTests"
        [ fuzz listOfPositionsWithLowCost "if all moves are available, a path is always found" <|
            \positions ->
                positions
                    |> List.map
                        (\( start, end ) ->
                            findPath
                                constantNeighbourCost
                                straightLineHeuristic
                                movesFrom
                                start
                                end
                        )
                    |> List.all (\x -> x /= Nothing)
                    |> Expect.equal True
        ]


straightLineHeuristicFuzzTests : Test
straightLineHeuristicFuzzTests =
    describe "straightLineHeuristicFuzzTests"
        [ fuzz tupleOfPositions "cost is always non-negative" <|
            \( start, end ) ->
                straightLineHeuristic start end |> Expect.atLeast 0.0
        ]



----------


listOfPositionsWithLowCost : Fuzzer (List ( Position, Position ))
listOfPositionsWithLowCost =
    listOfPositionsWithCost
        |> map (List.filter (\( _, cost ) -> cost <= 100.0))
        |> map (List.map Tuple.first)


listOfPositionsWithCost : Fuzzer (List ( ( Position, Position ), Float ))
listOfPositionsWithCost =
    listOfPositionTuples
        |> map (List.map (\pos -> ( pos, straightLineHeuristic (Tuple.first pos) (Tuple.second pos) )))


listOfPositionTuples : Fuzzer (List ( Position, Position ))
listOfPositionTuples =
    list tupleOfPositions


tupleOfPositions : Fuzzer ( Position, Position )
tupleOfPositions =
    tuple ( position, position )


position : Fuzzer Position
position =
    tuple ( int, int )


movesFrom : Position -> Set Position
movesFrom ( x, y ) =
    Set.fromList
        [ ( x - 1, y )
        , ( x + 1, y )
        , ( x, y - 1 )
        , ( x, y + 1 )
        , ( x - 1, y + 1 )
        , ( x - 1, y - 1 )
        , ( x + 1, y - 1 )
        , ( x + 1, y + 1 )
        ]


{-|

    - = highway (cost 1)
    M = mountain (cost 9)
    S = start (also, highway)
    T = target (also, highway)

    - - S - - -
    - M M M M M
    - - T - - -

-}
mountains : Set Position
mountains =
    Set.fromList
        [ ( 1, 1 )
        , ( 2, 1 )
        , ( 3, 1 )
        , ( 4, 1 )
        , ( 5, 1 )
        ]


mountainMapNeighbourCost : Position -> Position -> Float
mountainMapNeighbourCost from to =
    if Set.member to mountains then
        9

    else
        1
