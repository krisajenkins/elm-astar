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
        , straightLineCostFuzzTests
        ]


findPathTests : Test
findPathTests =
    describe "findPath"
        [ test "it finds a path" <|
            \() ->
                findPath straightLineCost movesFrom ( 0, 0 ) ( 2, 0 )
                    |> Expect.equal (Just [ ( 1, 0 ), ( 2, 0 ) ])
        ]


findPathFuzzTests : Test
findPathFuzzTests =
    describe "findPathFuzzTests"
        [ fuzz listOfPositionsWithLowCost "if all moves are available, a path is always found" <|
            \positions ->
                positions
                    |> List.map
                        (\( start, end ) ->
                            findPath straightLineCost movesFrom start end
                        )
                    |> List.all (\x -> x /= Nothing)
                    |> Expect.equal True
        ]


straightLineCostFuzzTests : Test
straightLineCostFuzzTests =
    describe "straightLineCostFuzzTests"
        [ fuzz tupleOfPositions "cost is always non-negative" <|
            \( start, end ) ->
                straightLineCost start end |> Expect.atLeast 0.0
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
        |> map (List.map (\pos -> ( pos, straightLineCost (Tuple.first pos) (Tuple.second pos) )))


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
