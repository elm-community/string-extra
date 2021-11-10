module LeftOfBackTest exposing (leftOfBackTest)

import Expect
import String.Extra exposing (leftOfBack)
import Test exposing (Test, test)


leftOfBackTest : Test
leftOfBackTest =
    test "leftOfBack" <|
        \() ->
            leftOfBack "_" "This_is_a_test_string"
                |> Expect.equal "This_is_a_test"
