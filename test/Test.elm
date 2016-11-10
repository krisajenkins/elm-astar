module Test exposing (..)

import AStarTests
import Legacy.ElmTest exposing (..)


tests : Test
tests =
    suite "All"
        [ AStarTests.tests ]


{-| Run the whole test suite.
-}
main : Program Never () msg
main =
    runSuite tests
