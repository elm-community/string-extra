module ClassifyTest exposing (classifyTest)

import Char
import Expect
import Fuzz exposing (..)
import Random
import Regex
import String exposing (replace, uncons)
import String.Extra exposing (..)
import Test exposing (..)
import TestData
import Tuple exposing (first, second)


classifyTest : Test
classifyTest =
    describe "classify"
        [ fuzz string "It does not contain non-word characters" <|
            \string ->
                classify string
                    |> Regex.contains (Regex.fromString "[\\W]" |> Maybe.withDefault Regex.never)
                    |> Expect.false "Non word characters detected"
        , fuzz TestData.randomStrings "It starts with an uppercase letter" <|
            \string ->
                string
                    |> classify
                    |> uncons
                    |> Maybe.map first
                    |> Expect.equal (string |> String.trim |> String.toUpper |> uncons |> Maybe.map first)
        , fuzz validWords "It is camelized once replaced non word charactes with a compatible string" <|
            \string ->
                string
                    |> classify
                    |> uncons
                    |> Maybe.map second
                    |> Expect.equal (string |> replace "." "-" |> camelize |> uncons |> Maybe.map second)
        ]


validWords : Fuzzer String
validWords =
    TestData.randomStringsWithCharGenerators
        [ Random.map Char.fromCode (Random.int 45 46)
        , Random.constant (Char.fromCode 95)
        ]
