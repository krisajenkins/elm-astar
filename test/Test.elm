module Test exposing (..)

import AStarTests
import ElmTest exposing (..)


tests : Test
tests =
    suite "All"
        [ AStarTests.tests ]


{-| Run the whole test suite.
-}
main : Program Never
main =
    runSuite tests
