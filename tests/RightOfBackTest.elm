module RightOfBackTest exposing (rightOfBackTest)

import Expect
import String.Extra exposing (rightOfBack)
import Test exposing (Test, test)


rightOfBackTest : Test
rightOfBackTest =
    test "rightOfBack" <|
        \() ->
            rightOfBack "_" "This_is_a_test_string"
                |> Expect.equal "string"
