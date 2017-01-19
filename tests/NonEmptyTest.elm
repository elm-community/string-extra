module NonEmptyTest exposing (nonEmptyTest)

import Expect
import String.Extra exposing (nonEmpty)
import Test exposing (..)


nonEmptyTest : Test
nonEmptyTest =
    describe "nonEmpty"
        [ test "Should result in a just when string has greater length than 0" <|
            \() ->
                nonEmpty "Hello world"
                    |> Expect.equal (Just "Hello world")
        , test "Should result in Nothing when an empty string is passed in" <|
            \() ->
                nonEmpty ""
                    |> Expect.equal Nothing
        ]
